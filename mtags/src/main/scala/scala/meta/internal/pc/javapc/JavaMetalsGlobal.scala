package scala.meta.internal.pc.javapc

import com.sun.source.tree.Tree
import com.sun.source.util.{JavacTask, TreePath}

import java.io.File
import java.net.URI
import java.nio.file.{FileSystem, FileSystems, Files}
import scala.jdk.CollectionConverters._
import java.util.ServiceLoader
import javax.tools.JavaCompiler
import scala.collection.mutable
import scala.util.Using

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

  def compilerTreeNode(
      scanner: JavaTreeScanner,
      position: CursorPosition
  ): TreePath = {
    val path = scanner.scan(scanner.root, position)
    lastVisitedParentTrees = scanner.lastVisitedParentTrees

    path
  }

  def getEndPosition(task: JavacTask, node: Tree): Long = {
    0
  }

  def allQualifiedNames(): List[String] = {
    jdkTopClasses
  }

  def classPathClasses(): List[String] = {
    Nil
  }

  private val JAVA_MODULES = List("java.base")

  val jdkTopClasses: List[String] = {

    val classes = mutable.Set[String]()
    val fs: FileSystem = FileSystems.getFileSystem(URI.create("jrt:/"))

    JAVA_MODULES.foreach { module =>
      val modulePath = fs.getPath(String.format("/modules/%s/", module))

      Using(Files.walk(modulePath)) { stream =>
        val it = stream.iterator()

        while (it.hasNext) {
          val classFile = it.next
          val relative = modulePath.relativize(classFile).toString
          if (relative.endsWith(".class") && !relative.contains("$")) {
            val trim = relative.substring(0, relative.length - ".class".length)
            val qualifiedName = trim.replace(File.separatorChar, '.')
            classes.add(qualifiedName)
          }
        }
      }
    }

    classes.toList
  }

}
