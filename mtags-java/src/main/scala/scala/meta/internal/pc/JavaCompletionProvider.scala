package scala.meta.internal.pc

import com.sun.source.tree.MemberSelectTree
import com.sun.source.util.{JavacTask, TreePath}
import org.eclipse.lsp4j.{CompletionItem, CompletionList}

import scala.jdk.CollectionConverters._
import scala.meta.pc.OffsetParams
import com.sun.source.tree.Tree.Kind._

import javax.lang.model.`type`.{ArrayType, DeclaredType, TypeVariable}
import javax.lang.model.element.TypeElement

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
        n.getLeaf.getKind match {
          case MEMBER_SELECT => completeMemberSelect(task, n)
          case _ => new CompletionList() // TODO: Handle all options
        }
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

  private def completeDeclaredType(
      task: JavacTask,
      declaredType: DeclaredType,
  ): CompletionList = {
    val members = task.getElements
      .getAllMembers(declaredType.asElement().asInstanceOf[TypeElement])
      .asScala
      .toList

    val start = inferIdentStart(params.offset(), params.text())
    val end = params.offset()

    val identifier = params.text().substring(start, end)

    val completionItems = members
      .filter(member => member.getSimpleName.toString.startsWith(identifier))
      .map { member =>
        new CompletionItem(
          member.getSimpleName.toString
        )
      }

    new CompletionList(completionItems.asJava)
  }

  private def completeArrayType(
      task: JavacTask,
      arrayType: ArrayType,
  ): CompletionList = {
    new CompletionList(
      List(
        new CompletionItem(
          "length"
        )
      ).asJava
    )
  }

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

}
