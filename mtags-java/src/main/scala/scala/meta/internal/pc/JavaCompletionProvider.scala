package scala.meta.internal.pc

import javax.lang.model.`type`.ArrayType
import javax.lang.model.`type`.DeclaredType
import javax.lang.model.`type`.TypeVariable
import javax.lang.model.element.Element
import javax.lang.model.element.ElementKind
import javax.lang.model.element.TypeElement

import scala.annotation.tailrec
import scala.jdk.CollectionConverters._

import scala.meta.pc.OffsetParams

import com.sun.source.tree.ClassTree
import com.sun.source.tree.CompilationUnitTree
import com.sun.source.tree.MemberSelectTree
import com.sun.source.tree.MethodTree
import com.sun.source.tree.Tree.Kind._
import com.sun.source.util.JavacTask
import com.sun.source.util.TreePath
import com.sun.source.util.Trees
import org.eclipse.lsp4j.CompletionItem
import org.eclipse.lsp4j.CompletionItemKind
import org.eclipse.lsp4j.CompletionList

class JavaCompletionProvider(
    compiler: JavaMetalsGlobal,
    params: OffsetParams,
) {

  def completions(): CompletionList = {
    val task: JavacTask = compiler.compilationTask(params.text())
    val scanner = compiler.scanner(task)
    val position =
      CursorPosition(params.offset(), params.offset(), params.offset())
    val node = compiler.compilerTreeNode(scanner, position)

    node match {
      case Some(n) =>
        val list =
          n.getLeaf.getKind match {
            case MEMBER_SELECT => completeMemberSelect(task, n)
            case IDENTIFIER => completeIdentifier(task, n)
            case _ => new CompletionList() // TODO: Handle all options
          }

        val keywordsCompletion = keywords(n)
        println(
          "keywordsCompletion: " + keywordsCompletion.getItems.asScala.length
        )
        println("list: " + list.getItems.asScala.length)
        val resultList =
          (list.getItems.asScala ++ keywordsCompletion.getItems.asScala).asJava

        new CompletionList(resultList)
      case None => new CompletionList()
    }
  }

  private def completeMemberSelect(
      task: JavacTask,
      path: TreePath,
  ): CompletionList = {
    val typeAnalyzer = new JavaTypeAnalyzer(task)
    val select = path.getLeaf.asInstanceOf[MemberSelectTree]
    val newPath = new TreePath(path, select.getExpression)
    val memberType = typeAnalyzer.typeMirror(newPath)

    memberType match {
      case dt: DeclaredType => completeDeclaredType(task, dt)
      case at: ArrayType => completeArrayType(task, at)
      case tv: TypeVariable => completeTypeVariable(task, tv)
    }
  }

  private def completeIdentifier(
      task: JavacTask,
      path: TreePath,
  ): CompletionList = {
    println("ident")
    new CompletionList(completeFromScope(task, path).asJava)
  }

  private def completeFromScope(
      task: JavacTask,
      path: TreePath,
  ): List[CompletionItem] = {
    val trees = Trees.instance(task)
    val scope = trees.getScope(path)

    val scopeCompletion = JavaScopeVisitor.scopeMembers(task, scope)
    val identifier = extractIdentifier

    scopeCompletion
      .map(completionItem)
      .filter(item => CompletionFuzzy.matches(identifier, item.getLabel))
  }

//  private def completeClassnames(
//    task: JavacTask,
//    path: TreePath,
//  ): List[CompletionItem] = {
//    val root = path.getCompilationUnit
//    val identifier = extractIdentifier
//
//    val packageName = Option(root.getPackageName).getOrElse("")
//
//    val packagePrivate = compiler.
//  }

  private def completeDeclaredType(
      task: JavacTask,
      declaredType: DeclaredType,
  ): CompletionList = {
    val members = task.getElements
      .getAllMembers(declaredType.asElement().asInstanceOf[TypeElement])
      .asScala
      .toList

    val identifier = extractIdentifier

    val completionItems = members
      .filter(member =>
        CompletionFuzzy.matches(identifier, member.getSimpleName.toString)
      )
      .map(completionItem)

    new CompletionList(completionItems.asJava)
  }

  private def extractIdentifier: String = {
    val start = inferIdentStart(params.offset(), params.text())
    val end = params.offset()

    params.text().substring(start, end)
  }

  private def keywords(path: TreePath): CompletionList = {
    val identifier = extractIdentifier
    val level = keywordLevel(path)

    println("KIND: " + path.getLeaf.getKind)
    println("LEVEL: " + level)
    println("IDENTIFIER: " + identifier)

    val completionItems =
      JavaKeyword.all
        .collect {
          case keyword
              if keyword.level == level && CompletionFuzzy.matches(
                identifier,
                keyword.name,
              ) =>
            keyword.name
        }
        .map { keyword =>
          val item = new CompletionItem(keyword)
          item.setKind(CompletionItemKind.Keyword)

          item
        }

    new CompletionList(completionItems.asJava)
  }

  @tailrec
  private def keywordLevel(path: TreePath): JavaKeyword.Level = {
    if (path == null) JavaKeyword.TopLevel
    else {
      path.getLeaf match {
        case _: MethodTree => JavaKeyword.MethodLevel
        case _: ClassTree => JavaKeyword.ClassLevel
        case _: CompilationUnitTree => JavaKeyword.TopLevel
        case _ => keywordLevel(path.getParentPath)
      }
    }
  }

  private def completeArrayType(
      task: JavacTask,
      arrayType: ArrayType,
  ): CompletionList = {
    val item = new CompletionItem("length")
    item.setKind(CompletionItemKind.Keyword)

    new CompletionList(
      List(item).asJava
    )
  }

  @tailrec
  private def completeTypeVariable(
      task: JavacTask,
      typeVariable: TypeVariable,
  ): CompletionList = {
    typeVariable.getUpperBound match {
      case dt: DeclaredType => completeDeclaredType(task, dt)
      case tv: TypeVariable => completeTypeVariable(task, tv)
      case _ => new CompletionList()
    }
  }

  private def inferIdentStart(pos: Int, text: String): Int = {
    var i = pos - 1
    while (i >= 0 && Character.isJavaIdentifierPart(text.charAt(i))) {
      i -= 1
    }
    i + 1
  }

  private def completionItem(element: Element): CompletionItem = {
    val item = new CompletionItem(
      element.getSimpleName.toString
    )

    val kind = completionKind(element.getKind)
    kind.foreach(item.setKind)

    item
  }

  private def completionKind(k: ElementKind): Option[CompletionItemKind] = {
    println("EL KIND: " + k)
    k match {
      case ElementKind.CLASS => Some(CompletionItemKind.Class)
      case ElementKind.ENUM => Some(CompletionItemKind.Enum)
      case ElementKind.ANNOTATION_TYPE => Some(CompletionItemKind.Interface)
      case ElementKind.INTERFACE => Some(CompletionItemKind.Interface)
      case ElementKind.CONSTRUCTOR => Some(CompletionItemKind.Constructor)
      case ElementKind.TYPE_PARAMETER => Some(CompletionItemKind.TypeParameter)
      case ElementKind.FIELD => Some(CompletionItemKind.Field)
      case ElementKind.PACKAGE => Some(CompletionItemKind.Module)
      case ElementKind.LOCAL_VARIABLE => Some(CompletionItemKind.Variable)
      case ElementKind.RESOURCE_VARIABLE => Some(CompletionItemKind.Variable)
      case ElementKind.PARAMETER => Some(CompletionItemKind.Property)
      case ElementKind.METHOD => Some(CompletionItemKind.Method)
      case _ => None
    }
  }

}
