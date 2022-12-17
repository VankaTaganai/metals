package tests.pc.javapc

import tests.BaseSuite

import scala.meta.internal.pc.javapc.JavaPresentationCompiler
import scala.meta.pc.PresentationCompiler

abstract class BaseJavaPCSuite extends BaseSuite { // todo: General Logic
  protected lazy val presentationCompiler: PresentationCompiler = {
    new JavaPresentationCompiler
  }

  def params(code: String, filename: String = "test.java"): (String, Int) = {
    val code2 = code.replace("@@", "")
    val offset = code.indexOf("@@")
    if (offset < 0) {
      fail("missing @@")
    }

    (code2, offset)
  }

  def hoverParams(
      code: String,
      filename: String = "test.java",
  ): (String, Int, Int) = {
    val code2 = code.replace("@@", "").replace("%<%", "").replace("%>%", "")
    val positionOffset =
      code.replace("%<%", "").replace("%>%", "").indexOf("@@")
    val startOffset = code.replace("@@", "").indexOf("%<%")
    val endOffset = code.replace("@@", "").replace("%<%", "").indexOf("%>%")
    (positionOffset, startOffset, endOffset) match {
      case (po, so, eo) if po < 0 && so < 0 && eo < 0 =>
        fail("missing @@ and (%<% and %>%)")
      case (_, so, eo) if so >= 0 && eo >= 0 =>
        (code2, so, eo)
      case (po, _, _) => (code2, po, po)
    }
  }
}
