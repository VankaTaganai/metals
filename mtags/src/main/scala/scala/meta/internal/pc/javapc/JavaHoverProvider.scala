package scala.meta.internal.pc.javapc

import com.sun.source.util.{DocTrees, JavacTask, Trees}
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
  ElementKind,
  ExecutableElement,
  Modifier,
  PackageElement,
  TypeElement,
  VariableElement
}
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.meta.internal.mtags.MtagsEnrichments.{
  XtensionOffsetParams,
  XtensionRangeParams,
  XtensionStringDoc
}
import scala.meta.internal.pc.HoverMarkup
import scala.meta.pc.{OffsetParams, RangeParams}

class JavaHoverProvider(
    compiler: JavaMetalsGlobal,
    params: OffsetParams
) {

  def hover(): Option[Hover] = params match {
    case range: RangeParams => range.trimWhitespaceInRange.flatMap(hoverOffset)
    case _ if params.isWhitespace => None
    case _ => hoverOffset(params)
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
      docs = documentation(element, task)
      hover <- hoverType(element, docs)
    } yield hover
  }

  def hoverType(element: Element, docs: String): Option[Hover] = {
    println("DOCS: " + docs)
    println("KIND: " + element.getKind)
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

  private def modifiersHover(element: Element): String = {
    val modifiers = element.getModifiers.asScala
    if (modifiers.isEmpty) "" else modifiers.mkString("", " ", " ")
  }

  private def classHover(element: TypeElement): String = {
    val modifiers = modifiersHover(element)
    val typeKind = element.getKind match {
      case CLASS => "class"
      case INTERFACE => "interface"
      case ENUM => "enum"
      case ANNOTATION_TYPE => "@interface"
//      case RECORD => "record"
      case _ => ???
    }

    val name = typeHover(element.asType())
    val superClass = typeHover(element.getSuperclass) match {
      case "java.lang.Object" => ""
      case sC => s" extends $sC"
    }

    val implementedClasses = element.getInterfaces.asScala.map(typeHover)
    val implementedClassesHover =
      if (implementedClasses.isEmpty) ""
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
    val d = DocTrees.instance(task).getDocCommentTree(element)

    if (d == null) ""
    else d.getFirstSentence.asScala.mkString("\n")
  }

}
