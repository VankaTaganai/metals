package scala.meta.internal.pc.javapc

import com.sun.source.tree.{
  CaseTree,
  CompilationUnitTree,
  ErroneousTree,
  IdentifierTree,
  ImportTree,
  MemberReferenceTree,
  MemberSelectTree,
  Tree
}
import com.sun.source.util.{JavacTask, TreePath, TreePathScanner, Trees}

class JavaTreeScanner(
    task: JavacTask,
    var root: CompilationUnitTree
) // todo: range
    extends TreePathScanner[TreePath, Int] {
  override def visitCompilationUnit(
      t: CompilationUnitTree,
      find: Int
  ): TreePath = {
    root = t
    reduce(super.visitCompilationUnit(t, find), getCurrentPath)
  }

  override def visitIdentifier(node: IdentifierTree, p: Int): TreePath = {
    val pos = Trees.instance(task).getSourcePositions
    val start = pos.getStartPosition(root, node)
    val end = pos.getEndPosition(root, node)

    if (start <= p && p <= end) {
      getCurrentPath
    } else {
      super.visitIdentifier(node, p)
    }
  }

  override def visitMemberSelect(node: MemberSelectTree, p: Int): TreePath = {
    val pos = Trees.instance(task).getSourcePositions
    val start = pos.getEndPosition(root, node.getExpression) + 1
    val end = pos.getEndPosition(root, node)

    if (start <= p && p <= end) {
      getCurrentPath
    } else {
      super.visitMemberSelect(node, p)
    }
  }

  override def visitMemberReference(
      node: MemberReferenceTree,
      p: Int
  ): TreePath = {
    val pos = Trees.instance(task).getSourcePositions
    val start = pos.getEndPosition(root, node.getQualifierExpression) + 2
    val end = pos.getEndPosition(root, node)

    if (start <= p && p <= end) {
      getCurrentPath
    } else {
      super.visitMemberReference(node, p)
    }
  }

  override def visitCase(node: CaseTree, p: Int): TreePath = {
    val pos = Trees.instance(task).getSourcePositions
    val start = pos.getStartPosition(root, node) + "case".length
    val end = pos.getEndPosition(root, node.getExpressions.get(0))

    if (start <= p && p <= end) {
      getCurrentPath.getParentPath
    } else {
      super.visitCase(node, p)
    }
  }

  override def visitImport(node: ImportTree, p: Int): TreePath = {
    val pos = Trees.instance(task).getSourcePositions
    val start = pos.getStartPosition(root, node.getQualifiedIdentifier)
    val end = pos.getEndPosition(root, node.getQualifiedIdentifier)

    if (start <= p && p <= end) {
      getCurrentPath
    } else {
      super.visitImport(node, p)
    }
  }

  override def visitErroneous(node: ErroneousTree, p: Int): TreePath =
    scan(node.getErrorTrees, p)

  override def reduce(a: TreePath, b: TreePath): TreePath = {
    if (a != null) return a
    b
  }
}
