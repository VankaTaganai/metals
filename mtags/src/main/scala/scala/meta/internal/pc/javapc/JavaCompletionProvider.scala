package scala.meta.internal.pc.javapc

import com.sun.source.tree.MemberSelectTree
import com.sun.source.util.{JavacTask, TreePath}
import org.eclipse.lsp4j.{CompletionItem, CompletionList}

import scala.jdk.CollectionConverters._
import java.util.ServiceLoader
import javax.tools.JavaCompiler
import scala.meta.pc.OffsetParams
import com.sun.source.tree.Tree.Kind._

import javax.lang.model.`type`.{ArrayType, DeclaredType, TypeVariable}
import javax.lang.model.element.TypeElement

class JavaCompletionProvider(
    params: OffsetParams
) {
  // TODO: move compiler to compiler wrapper
  private val COMPILER: JavaCompiler =
    ServiceLoader.load(classOf[JavaCompiler]).iterator.next

  def completions(): CompletionList = {
    val javaFileObject = SourceJavaFileObject.make(params.text())

    val task: JavacTask = COMPILER
      .getTask(
        null,
        null,
        null,
        null,
        null,
        List(javaFileObject).asJava
      )
      .asInstanceOf[JavacTask]

    val elems = task.parse()
    task.analyze()
    val root = elems.iterator().next()

    val curNode = new JavaTreeScanner(task, root).scan(root, params.offset())

    curNode.getLeaf.getKind match {
      case MEMBER_SELECT => completeMemberSelect(task, curNode)
      case _ => ??? // TODO: Handle all options
    }
  }

  private def completeMemberSelect(
      task: JavacTask,
      path: TreePath
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
      declaredType: DeclaredType
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
      arrayType: ArrayType
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
      typeVariable: TypeVariable
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
