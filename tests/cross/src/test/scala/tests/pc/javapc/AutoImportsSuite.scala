package tests.pc.javapc

import coursierapi.{Dependency, Fetch}
import tests.Assertions

import java.io.{File, IOException}
import java.net.URI
import java.nio.file.{FileSystem, FileSystems, Files, Path}
import java.util
import java.util.{HashSet, Iterator}
import java.util.stream.Stream
import scala.collection.mutable
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.meta.internal.pc.AutoImportsResultImpl
import scala.util.Using

class AutoImportsSuite extends munit.FunSuite with Assertions {
  private val JAVA_MODULES = List("java.base")

  test("test") {
    val name = "List"

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

    val res = classes.toList.filter(_.endsWith(s".$name"))

    println(res)

//    val classPath2 = System.getProperty("java.class.path")
//    println(classPath2)

//    val dep = Dependency.of("java.base", "base", "17.0")
//    val fetch = Fetch.create().withDependencies(dep).fetch().asScala.map(_.toPath)
//
//    println(fetch)
  }
}
