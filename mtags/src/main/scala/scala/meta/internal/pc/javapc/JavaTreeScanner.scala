package scala.meta.internal.pc.javapc

import com.sun.source.tree.{
  CaseTree,
  CompilationUnitTree,
  ErroneousTree,
  IdentifierTree,
  ImportTree,
  MemberReferenceTree,
  MemberSelectTree,
  PackageTree,
  Tree
}
import com.sun.source.util.{JavacTask, TreePath, TreePathScanner, Trees}

import scala.collection.immutable.Nil

class JavaTreeScanner(
    task: JavacTask,
    var root: CompilationUnitTree
) extends TreePathScanner[TreePath, CursorPosition] {

  var lastVisitedParentTrees: List[Tree] = Nil

  override def visitCompilationUnit(
      t: CompilationUnitTree,
      p: CursorPosition
  ): TreePath = {
    root = t
    reduce(super.visitCompilationUnit(t, p), getCurrentPath)
  }

  override def visitIdentifier(
      node: IdentifierTree,
      p: CursorPosition
  ): TreePath = {
    val pos = Trees.instance(task).getSourcePositions
    val start = pos.getStartPosition(root, node)
    val end = pos.getEndPosition(root, node)

    lastVisitedParentTrees = node :: lastVisitedParentTrees
    if (start <= p.start && p.end <= end) {
      getCurrentPath
    } else {
      super.visitIdentifier(node, p)
    }
  }

  override def visitMemberSelect(
      node: MemberSelectTree,
      p: CursorPosition
  ): TreePath = {
    val pos = Trees.instance(task).getSourcePositions
    val start = pos.getEndPosition(root, node.getExpression) + 1
    val end = pos.getEndPosition(root, node)

    lastVisitedParentTrees = node :: lastVisitedParentTrees
    if (start <= p.start && p.end <= end) {
      getCurrentPath
    } else {
      super.visitMemberSelect(node, p)
    }
  }

  override def visitMemberReference(
      node: MemberReferenceTree,
      p: CursorPosition
  ): TreePath = {
    val pos = Trees.instance(task).getSourcePositions
    val start = pos.getEndPosition(root, node.getQualifierExpression) + 2
    val end = pos.getEndPosition(root, node)

    lastVisitedParentTrees = node :: lastVisitedParentTrees
    if (start <= p.start && p.end <= end) {
      getCurrentPath
    } else {
      super.visitMemberReference(node, p)
    }
  }

  override def visitCase(node: CaseTree, p: CursorPosition): TreePath = {
    val pos = Trees.instance(task).getSourcePositions
    val start = pos.getStartPosition(root, node) + "case".length
    val end = pos.getEndPosition(root, node.getExpressions.get(0))

    lastVisitedParentTrees = node :: lastVisitedParentTrees
    if (start <= p.start && p.end <= end) {
      getCurrentPath.getParentPath
    } else {
      super.visitCase(node, p)
    }
  }

  override def visitImport(node: ImportTree, p: CursorPosition): TreePath = {
    val pos = Trees.instance(task).getSourcePositions
    val start = pos.getStartPosition(root, node.getQualifiedIdentifier)
    val end = pos.getEndPosition(root, node.getQualifiedIdentifier)

    lastVisitedParentTrees = node :: lastVisitedParentTrees
    if (start <= p.start && p.end <= end) {
      getCurrentPath
    } else {
      super.visitImport(node, p)
    }
  }

  override def visitErroneous(
      node: ErroneousTree,
      p: CursorPosition
  ): TreePath =
    scan(node.getErrorTrees, p)

  override def visitPackage(node: PackageTree, p: CursorPosition): TreePath = {
    val pos = Trees.instance(task).getSourcePositions
    val start = pos.getStartPosition(root, node.getPackageName)
    val end = pos.getEndPosition(root, node.getPackageName)

    lastVisitedParentTrees = node :: lastVisitedParentTrees
    if (start <= p.start && p.end <= end) {
      getCurrentPath
    } else {
      super.visitPackage(node, p)
    }
  }

  override def reduce(a: TreePath, b: TreePath): TreePath = {
    if (a != null) return a
    b
  }

  def getEndPosition(node: Tree): Long = {
    val pos = Trees.instance(task).getSourcePositions

    pos.getEndPosition(root, node)
  }
}
