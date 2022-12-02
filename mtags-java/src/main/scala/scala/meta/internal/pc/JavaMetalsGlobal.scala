package scala.meta.internal.pc

import java.util.ServiceLoader
import javax.tools.JavaCompiler

import scala.jdk.CollectionConverters._

import scala.meta.pc.PresentationCompilerConfig
import scala.meta.pc.SymbolSearch

import com.sun.source.util.JavacTask
import com.sun.source.util.TreePath

class JavaMetalsGlobal(
    val search: SymbolSearch,
    val metalsConfig: PresentationCompilerConfig,
) {

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
        List(javaFileObject).asJava,
      )
      .asInstanceOf[JavacTask]
  }

  var lastVisitedParentTrees: List[TreePath] = Nil

  def scanner(task: JavacTask): JavaTreeScanner = {
    val elems = task.parse()
    task.analyze()
    val root = elems.iterator().next()

    new JavaTreeScanner(task, root)
  }

  def compilerTreeNode(
      scanner: JavaTreeScanner,
      position: CursorPosition,
  ): Option[TreePath] = {
    scanner.scan(scanner.root, position)
    lastVisitedParentTrees = scanner.lastVisitedParentTrees

    lastVisitedParentTrees.headOption
  }
}
