package scala.meta.internal.pc.javapc

import com.sun.source.tree.{
  AssignmentTree,
  CaseTree,
  ClassTree,
  CompilationUnitTree,
  CompoundAssignmentTree,
  ErroneousTree,
  IdentifierTree,
  ImportTree,
  LiteralTree,
  MemberReferenceTree,
  MemberSelectTree,
  NewClassTree,
  PackageTree,
  Tree,
  VariableTree
}
import com.sun.source.util.{JavacTask, TreePath, TreePathScanner, Trees}

import scala.collection.immutable.Nil

class JavaTreeScanner(
    task: JavacTask,
    var root: CompilationUnitTree
) extends TreePathScanner[TreePath, CursorPosition] {

  var lastVisitedParentTrees: List[TreePath] = Nil
  var lastVisitedPackageTrees: List[PackageTree] = Nil

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

    println("NODE: " + node.getName)
    println(start + " " + end)
    if (start <= p.start && p.end <= end) {
      lastVisitedParentTrees = getCurrentPath :: lastVisitedParentTrees
    }
    super.visitIdentifier(node, p)
  }

  override def visitMemberSelect(
      node: MemberSelectTree,
      p: CursorPosition
  ): TreePath = {
    val pos = Trees.instance(task).getSourcePositions
    val start = pos.getEndPosition(root, node.getExpression) + 1
    val end = pos.getEndPosition(root, node)

    println("NODE: " + node.getExpression)
    println(start + " " + end)
    if (start <= p.start && p.end <= end) {
      lastVisitedParentTrees = getCurrentPath :: lastVisitedParentTrees
    }
    super.visitMemberSelect(node, p)

  }

  override def visitMemberReference(
      node: MemberReferenceTree,
      p: CursorPosition
  ): TreePath = {
    val pos = Trees.instance(task).getSourcePositions
    val start = pos.getEndPosition(root, node.getQualifierExpression) + 2
    val end = pos.getEndPosition(root, node)
    println("NODE: " + node.getQualifierExpression)
    println(start + " " + end)
    if (start <= p.start && p.end <= end) {
      lastVisitedParentTrees = getCurrentPath :: lastVisitedParentTrees
    }
    super.visitMemberReference(node, p)
  }

  override def visitCase(node: CaseTree, p: CursorPosition): TreePath = {
    val pos = Trees.instance(task).getSourcePositions
    val start = pos.getStartPosition(root, node) + "case".length
    val end = pos.getEndPosition(root, node.getExpressions.get(0))

    if (start <= p.start && p.end <= end) {
      lastVisitedParentTrees =
        getCurrentPath.getParentPath :: lastVisitedParentTrees
      getCurrentPath.getParentPath
    }
    super.visitCase(node, p)
  }

  override def visitImport(node: ImportTree, p: CursorPosition): TreePath = {
    val pos = Trees.instance(task).getSourcePositions
    val start = pos.getStartPosition(root, node.getQualifiedIdentifier)
    val end = pos.getEndPosition(root, node.getQualifiedIdentifier)

    if (start <= p.start && p.end <= end) {
      lastVisitedParentTrees = getCurrentPath :: lastVisitedParentTrees
    }
    super.visitImport(node, p)
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

    lastVisitedPackageTrees = node :: lastVisitedPackageTrees
    if (start <= p.start && p.end <= end) {
      lastVisitedParentTrees = getCurrentPath :: lastVisitedParentTrees
    }
    super.visitPackage(node, p)
  }

  override def visitVariable(
      node: VariableTree,
      p: CursorPosition
  ): TreePath = {
    val pos = Trees.instance(task).getSourcePositions
    val start = pos.getStartPosition(root, node)
    val end = pos.getEndPosition(root, node)

    println("NODE VARIABLE: " + node.getName)
    println(start + " " + end)
    println("CUR POS: " + p.start)
    if (node.getNameExpression != null) {
      println("child: " + node.getNameExpression.getKind)
    } else {
      println("GG")
    }

    if (start <= p.start && p.end <= end) {
      println("CHECK")
      lastVisitedParentTrees = getCurrentPath :: lastVisitedParentTrees
    }
    super.visitVariable(node, p)
  }

  override def visitClass(node: ClassTree, p: CursorPosition): TreePath = {
    visitNode(node, p, super.visitClass)
  }

  override def visitNewClass(
      node: NewClassTree,
      p: CursorPosition
  ): TreePath = {
    val pos = Trees.instance(task).getSourcePositions
    val start = pos.getStartPosition(root, node.getIdentifier)
    val end = pos.getEndPosition(root, node.getIdentifier)

    if (start <= p.start && p.end <= end) {
      lastVisitedParentTrees = getCurrentPath :: lastVisitedParentTrees
      getCurrentPath
    } else {
      super.visitNewClass(node, p)
      visitNode(node, p, super.visitNewClass)
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

  private def visitNode[N <: Tree](
      node: N,
      p: CursorPosition,
      traverse: (N, CursorPosition) => TreePath
  ): TreePath = {
    val pos = Trees.instance(task).getSourcePositions
    val start = pos.getStartPosition(root, node)
    val end = pos.getEndPosition(root, node)

    println("NODE VARIABLE: " + node.getKind)
    println(start + " " + end)

    if (start <= p.start && p.end <= end) {
      lastVisitedParentTrees = getCurrentPath :: lastVisitedParentTrees
    }

    traverse(node, p)
  }
}
