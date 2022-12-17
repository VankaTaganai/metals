package tests.pc.javapc

import munit.{Location, TestOptions}
import tests.TestHovers

import java.nio.file.Paths
import scala.meta.internal.metals.{CompilerOffsetParams, CompilerRangeParams}
import scala.meta.internal.mtags.MtagsEnrichments._

class BaseJavaHoverSuite extends BaseJavaPCSuite with TestHovers { // todo: General Logic

  def check(
      testOpt: TestOptions,
      original: String,
      expected: String,
      includeRange: Boolean = false,
      automaticPackage: Boolean = true,
      compat: Map[String, String] = Map.empty,
  )(implicit loc: Location): Unit = {
    test(testOpt) {
      val filename = "Hover.java"
      val pkg = scala.meta.Term.Name(testOpt.name).syntax
      val noRange = original
        .replace("<<", "")
        .replace(">>", "")
      val packagePrefix =
        if (automaticPackage) s"package $pkg\n"
        else ""
      val codeOriginal = packagePrefix + noRange
      val (code, so, eo) = hoverParams(codeOriginal, filename)
      val pcParams = if (so == eo) {
        CompilerOffsetParams(Paths.get(filename).toUri, code, so)
      } else {
        CompilerRangeParams(Paths.get(filename).toUri, code, so, eo)
      }
      val hover = presentationCompiler
        .hover(pcParams)
        .get()

      val obtained: String = renderAsString(code, hover.asScala, includeRange)

      assertNoDiff(
        obtained,
        expected,
      )
    }
  }
}
