package tests.pc.javapc

import munit.Location
import tests.BuildInfoVersions

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.meta.dialects
import scala.meta.internal.metals.CompilerOffsetParams
import scala.meta.pc.AutoImportsResult
import scala.util.control.NonFatal

trait BaseJavaAutoImportsSuite extends BaseJavaPCSuite {
  val isExtensionMethods: Boolean = false

  def check(
      name: String,
      original: String,
      expected: String,
      compat: Map[String, String] = Map.empty,
  )(implicit loc: Location): Unit =
    test(name) {
      val imports = getAutoImports(original, "A.scala")
      val obtained = imports.map(_.packageName()).mkString("\n")
      assertNoDiff(
        obtained,
        expected,
      )
    }

  def getAutoImports(
      original: String,
      filename: String,
  ): List[AutoImportsResult] = {
    val (code, symbol, offset) = params(original)
    val result = presentationCompiler
      .autoImports(
        symbol,
        CompilerOffsetParams(
          Paths.get(filename).toUri,
          code,
          offset,
        ),
        isExtensionMethods,
      )
      .get()
    result.asScala.toList
  }

  def params(
      code: String
  ): (String, String, Int) = {
    val filename = "test.java"
    val targetRegex = "<<(.+)>>".r
    val target = targetRegex.findAllMatchIn(code).toList match {
      case Nil => fail("Missing <<target>>")
      case t :: Nil => t.group(1)
      case _ => fail("Multiple <<targets>> found")
    }
    val code2 = code.replace("<<", "").replace(">>", "")
    val offset = code.indexOf("<<") + target.length()
    val file = tmp.resolve(filename)
    Files.write(file.toNIO, code2.getBytes(StandardCharsets.UTF_8))
    (code2, target, offset)
  }
}
