package scala.meta.internal.pc

import com.sun.source.tree.CaseTree
import com.sun.source.tree.ExpressionTree

trait JavaCompatTreeOps {
  def caseNodeExpression(node: CaseTree): ExpressionTree
}
