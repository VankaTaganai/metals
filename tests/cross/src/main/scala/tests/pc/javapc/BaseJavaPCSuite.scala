package tests.pc.javapc

import tests.BaseSuite

import scala.meta.internal.pc.javapc.JavaPresentationCompiler
import scala.meta.pc.PresentationCompiler

abstract class BaseJavaPCSuite extends BaseSuite {
  protected lazy val presentationCompiler: PresentationCompiler = {
    new JavaPresentationCompiler
  }

  def params(code: String, filename: String = "test.scala"): (String, Int) = {
    val code2 = code.replace("@@", "")
    val offset = code.indexOf("@@")
    if (offset < 0) {
      fail("missing @@")
    }

    (code2, offset)
  }
}
