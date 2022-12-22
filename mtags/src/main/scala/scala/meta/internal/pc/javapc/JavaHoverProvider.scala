package scala.meta.internal.pc.javapc

import com.sun.source.util.{JavacTask, Trees}
import org.eclipse.lsp4j.Hover

import javax.lang.model.element.{Element, VariableElement}
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
    val node = compiler.compilerTreeNode(scanner, params.offset())

    val element = Trees.instance(task).getElement(node)

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
