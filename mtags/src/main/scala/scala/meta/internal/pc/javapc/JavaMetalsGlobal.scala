package scala.meta.internal.pc.javapc

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

  def compilerTreeNode(task: JavacTask, offset: Int): TreePath = {
    val elems = task.parse()
    task.analyze()
    val root = elems.iterator().next()

    new JavaTreeScanner(task, root).scan(root, offset)
  }

}
