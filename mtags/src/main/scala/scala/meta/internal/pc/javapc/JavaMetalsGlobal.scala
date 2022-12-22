package scala.meta.internal.pc.javapc

import com.sun.source.tree.Tree
import com.sun.source.util.{JavacTask, TreePath}

import scala.jdk.CollectionConverters._
import java.util.ServiceLoader
import javax.tools.JavaCompiler

class JavaMetalsGlobal {

  private val COMPILER: JavaCompiler =
    ServiceLoader.load(classOf[JavaCompiler]).iterator.next

  def compilationTask(sourceCode: String): JavacTask = {
    val javaFileObject = SourceJavaFileObject.make(sourceCode)

    COMPILER
      .getTask(
        null,
        null,
        null,
        null,
        null,
        List(javaFileObject).asJava
      )
      .asInstanceOf[JavacTask]
  }

  var lastVisitedParentTrees: List[Tree] = Nil

  def scanner(task: JavacTask): JavaTreeScanner = {
    val elems = task.parse()
    task.analyze()
    val root = elems.iterator().next()

    new JavaTreeScanner(task, root)
  }

  def compilerTreeNode(scanner: JavaTreeScanner, offset: Int): TreePath = {
    val path = scanner.scan(scanner.root, offset)
    lastVisitedParentTrees = scanner.lastVisitedParentTrees

    path
  }

  def getEndPosition(task: JavacTask, node: Tree): Long = {
    new
  }

}
