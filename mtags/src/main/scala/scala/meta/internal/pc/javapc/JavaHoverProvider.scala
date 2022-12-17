package scala.meta.internal.pc.javapc

import com.sun.source.util.{JavacTask, Trees}
import org.eclipse.lsp4j.Hover

import scala.jdk.CollectionConverters._
import java.util.ServiceLoader
import javax.lang.model.element.{Element, ExecutableElement, VariableElement}
import javax.tools.JavaCompiler
import scala.meta.internal.mtags.MtagsEnrichments.{
  XtensionOffsetParams,
  XtensionRangeParams,
  XtensionStringDoc
}
import scala.meta.internal.pc.HoverMarkup
import scala.meta.pc.{OffsetParams, RangeParams}

class JavaHoverProvider(params: OffsetParams) {
  // TODO: move compiler to compiler wrapper
  private val COMPILER: JavaCompiler =
    ServiceLoader.load(classOf[JavaCompiler]).iterator.next

  def hover(): Option[Hover] = params match {
    case range: RangeParams => range.trimWhitespaceInRange.flatMap(hoverOffset)
    case _ if params.isWhitespace => None
    case _ => hoverOffset(params)
  }

  def hoverOffset(params: OffsetParams): Option[Hover] = {
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
    val element = Trees.instance(task).getElement(curNode)

    hoverType(element)
  }

  def hoverType(element: Element): Option[Hover] = {
    element match {
      case e: VariableElement =>
        val prettyType = e.asType().accept(new JavaTypeVisitor(), null)

        Some(new Hover(HoverMarkup.javaHoverMarkup(prettyType).toMarkupContent))
      case _ => None
    }
  }

}
