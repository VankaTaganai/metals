package scala.meta.internal.pc

import com.sun.source.util.{DocTrees, JavacTask, Trees}
import com.sun.tools.javac.code.Type
import org.eclipse.lsp4j.Hover

import javax.lang.model.`type`.TypeMirror
import javax.lang.model.element.ElementKind.{
  ANNOTATION_TYPE,
  CLASS,
  CONSTRUCTOR,
  ENUM,
  INTERFACE,
  RECORD
}
import javax.lang.model.element.{
  Element,
  ExecutableElement,
  Modifier,
  PackageElement,
  TypeElement,
  VariableElement
}
import scala.jdk.CollectionConverters.{CollectionHasAsScala, SeqHasAsJava}
import scala.meta.internal.mtags.MtagsEnrichments.{
  XtensionRangeParams,
  XtensionStringDoc
}
import scala.meta.internal.pc.HoverMarkup
import scala.meta.pc.{OffsetParams, ParentSymbols, RangeParams}
import com.sun.tools.javac.code.Symbol
import com.sun.tools.javac.code.Symbol._
import com.sun.tools.javac.code.Type.ClassType

import scala.meta.internal.mtags.MtagsEnrichments._
import java.util
import scala.jdk.OptionConverters.RichOptional

class JavaHoverProvider(
    compiler: JavaMetalsGlobal,
    params: OffsetParams
) {

  def hover(): Option[Hover] = params match {
    case range: RangeParams => range.trimWhitespaceInRange.flatMap(hoverOffset)
    case _ if isWhitespace => None
    case _ => hoverOffset(params)
  }

  private def isWhitespace: Boolean = {
    params.offset() < 0 ||
    params.offset() >= params.text().length ||
    params.text().charAt(params.offset()).isWhitespace
  }

  def hoverOffset(params: OffsetParams): Option[Hover] = {
    val task: JavacTask = compiler.compilationTask(params.text())
    val scanner = compiler.scanner(task)
    val position = params match {
      case p: RangeParams =>
        CursorPosition(p.offset(), p.offset(), p.endOffset())
      case p: OffsetParams => CursorPosition(p.offset(), p.offset(), p.offset())
    }

    val node = compiler.compilerTreeNode(scanner, position)

    for {
      n <- node
      element = Trees.instance(task).getElement(n)
      docs =
        if (compiler.metalsConfig.isHoverDocumentationEnabled)
          documentation(element, task)
        else ""
      hover <- hoverType(element, docs)
    } yield hover
  }

  def hoverType(element: Element, docs: String): Option[Hover] = {
    println("DOCS: " + docs)
//    println("KIND: " + element.getKind)
    println("NAME: " + element.getSimpleName)
    println("TYPE: " + element.getClass)
    println("TREE: " + compiler.lastVisitedParentTrees)
    element match {
      case e: VariableElement =>
        val prettyType = variableHover(e)

        Some(
          new Hover(
            HoverMarkup.javaHoverMarkup("", prettyType, docs).toMarkupContent
          )
        )
      case e: TypeElement =>
        println("TYPE ELEMENT: " + e.asType().getKind)
        val prettyType = classHover(e)
        println("PRETTY: " + prettyType)

        Some(
          new Hover(
            HoverMarkup.javaHoverMarkup("", prettyType, docs).toMarkupContent
          )
        )
      case e: ExecutableElement =>
        val prettyType = executableHover(e)

        Some(
          new Hover(
            HoverMarkup.javaHoverMarkup("", prettyType, docs).toMarkupContent
          )
        )
      case e: PackageElement =>
        val prettyType = packageHover(e)
        Some(
          new Hover(
            HoverMarkup.javaHoverMarkup("", prettyType, docs).toMarkupContent
          )
        )
      case _ => None
    }
  }

  private def typeHover(t: TypeMirror): String =
    t.accept(new JavaTypeVisitor(), null)

  private def modifiersHover(
      element: Element,
      filter: Set[Modifier] = Set()
  ): String = {
    val modifiers =
      element.getModifiers.asScala.filterNot(m => filter.contains(m))
    if (modifiers.isEmpty) "" else modifiers.mkString("", " ", " ")
  }

  private def classHover(element: TypeElement): String = {
    val (typeKind, fModifiers) = element.getKind match {
      case CLASS => ("class", Set.empty[Modifier])
      case INTERFACE => ("interface", Set(Modifier.ABSTRACT))
      case ENUM => ("enum", Set.empty[Modifier])
      case ANNOTATION_TYPE => ("@interface", Set.empty[Modifier])
      case _ => ("", Set.empty[Modifier])
    }

    val modifiers = modifiersHover(element, fModifiers)

    val name = typeHover(element.asType())
    val superClass = typeHover(element.getSuperclass) match {
      case sC if sC == "java.lang.Object" || sC == "none" => ""
      case sC => s" extends $sC"
    }

    val implementedClasses = element.getInterfaces.asScala.map(typeHover)
    val implementedClassesHover =
      if (implementedClasses.isEmpty) ""
      else if (element.getKind == INTERFACE)
        implementedClasses.mkString(" extends ", ", ", "")
      else implementedClasses.mkString(" implements ", ", ", "")

    s"$modifiers$typeKind $name$superClass$implementedClassesHover"
  }

  private def argumentHover(element: VariableElement): String = {
    val argType = typeHover(element.asType())
    val argName = element.getSimpleName

    s"$argType $argName"
  }

  private def executableHover(element: ExecutableElement): String = {
    val modifiers = modifiersHover(element)
    val returnType = typeHover(element.asType())
    val functionName =
      if (element.getKind == CONSTRUCTOR)
        element.getEnclosingElement.getSimpleName
      else element.getSimpleName
    val arguments =
      element.getParameters.asScala.map(argumentHover).mkString(", ")

    val throws = element.getThrownTypes.asScala
    val throwsHover =
      if (throws.isEmpty) ""
      else
        throws
          .map(t => t.accept(new JavaTypeVisitor(), null))
          .mkString(" throws ", ", ", "")

    s"$modifiers$returnType $functionName($arguments)$throwsHover"
  }

  private def packageHover(element: PackageElement): String =
    s"package ${element.getQualifiedName}"

  private def variableHover(element: VariableElement): String = {
    val modifiers = modifiersHover(element)
    val variableType = typeHover(element.asType())
    val name = element.getSimpleName

    s"$modifiers$variableType $name"
  }

  private def documentation(element: Element, task: JavacTask): String = {
    element match {
      case symbol: Symbol =>
        val sym = semanticdbSymbol(symbol)
        println("SEMANITCDB: " + sym)
        compiler.search
          .documentation(
            sym,
            new ParentSymbols {
              override def parents(): util.List[String] = symbol.owner
                .asType() match {
                case cType: ClassType => overriddenSymbols(symbol, cType)
                case _ => util.List.of()
              }
            }
          )
          .toScala
          .map(_.docstring())
          .getOrElse("")
      case _ => ""
    }
  }

  private def overriddenSymbols(
      symbol: Symbol,
      cType: ClassType
  ): util.List[String] = {
    val types = baseSymbols(cType)

    (for {
      t <- types
      s <- t.tsym.getEnclosedElements.asScala
      if s.getSimpleName == symbol.getSimpleName
    } yield semanticdbSymbol(s)).asJava
  }

  private def baseSymbols(cType: ClassType): List[ClassType] = {
    val superType = Option(cType.supertype_field)
    val inheritedTypes =
      if (cType.interfaces_field == null) List()
      else cType.interfaces_field.asScala

    val baseTypes = (superType ++ inheritedTypes).collect { case c: ClassType =>
      c
    }

    baseTypes.flatMap(c => c :: baseSymbols(c)).toList
  }

  private def semanticdbSymbol(symbol: Symbol): String = {

    def descriptors(symbol: Symbol): List[Descriptor] = {
      if (symbol == null || symbol.name.toString == "") Descriptor.None :: Nil
      else {
        val owner = descriptors(symbol.owner)
        val desc = {
          val name = symbol.name.toString
          println("KIND: " + symbol.kind)
          println("NAME: " + name)

          symbol match {
            case _: PackageSymbol => Descriptor.Package(name)
            case m: MethodSymbol =>
              Descriptor.Method(name, disambiguator(m))
            case _: ClassSymbol => Descriptor.Class(name)
            case _: TypeVariableSymbol => Descriptor.TypeVariable(name)
            case _: VarSymbol => Descriptor.Var(name)
            case _ => Descriptor.None
          }
        }

        desc :: owner
      }
    }

    val decs = descriptors(symbol).filter(_ != Descriptor.None).reverse

    (decs match {
      case Nil => List.empty[Descriptor]
      case d @ (Descriptor.Package(_) :: _) => d
      case d => Descriptor.Package("_empty_") :: d
    }).mkString("")
  }

  private def disambiguator(symbol: Symbol.MethodSymbol): String = {
    val methods = symbol.owner.getEnclosedElements.asScala.collect {
      case e: ExecutableElement if e.getSimpleName == symbol.name => e
    }

    val index = methods.zipWithIndex.collectFirst {
      case (e, i) if e.equals(symbol) => i
    }

    index match {
      case Some(i) => if (i == 0) "()" else s"(+$i)"
      case None => "()"
    }
  }

  object Symbols {
    val None: String = ""
    val RootPackage: String = "_root_/"
  }

  sealed trait Descriptor {
    import Descriptor._

    def value: String
    override def toString: String = {
      this match {
        case None => ""
        case Package(value) => s"${encode(value)}/"
        case Method(value, disambiguator) => s"${encode(value)}$disambiguator."
        case Class(value) => s"${encode(value)}#"
        case TypeVariable(value) => s"[${encode(value)}]"
        case Var(value) => s"${encode(value)}."
      }
    }
  }
  object Descriptor {
    case object None extends Descriptor {
      override val value: String = ""
    }
    final case class Package(value: String) extends Descriptor
    final case class Method(value: String, disambiguator: String)
        extends Descriptor
    final case class Class(value: String) extends Descriptor
    final case class TypeVariable(value: String) extends Descriptor
    final case class Var(value: String) extends Descriptor

    def encode(value: String): String = {
      val v = (if (value == "") {
                 ""
               } else {
                 val (start, parts) = (value.head, value.tail)
                 val isStartOk = Character.isJavaIdentifierStart(start)
                 val isPartsOk = parts.forall(Character.isJavaIdentifierPart)
                 if (isStartOk && isPartsOk) value
                 else "`" + value + "`"
               })

      println("VAL: " + v)
      v
    }
  }
}
