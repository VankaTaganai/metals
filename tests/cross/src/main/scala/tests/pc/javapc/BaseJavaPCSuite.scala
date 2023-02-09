package tests.pc.javapc

import coursierapi.{Fetch, Repository}
import tests.{
  BaseSuite,
  DelegatingGlobalSymbolIndex,
  TestingSymbolSearch,
  TestingWorkspaceSearch,
}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.meta.dialects
import scala.meta.internal.metals.{
  ClasspathSearch,
  Docstrings,
  ExcludedPackagesHandler,
  JdkSources,
  PackageIndex,
}
import scala.meta.internal.pc.PresentationCompilerConfigImpl
import scala.meta.internal.pc.javapc.JavaPresentationCompiler
import scala.meta.io.AbsolutePath
import scala.meta.pc.PresentationCompiler
import scala.util.control.NonFatal

abstract class BaseJavaPCSuite extends BaseSuite { // todo: General Logic

  val documentationHoverEnabled = false

  val allRepos: Seq[Repository] =
    Repository.defaults().asScala.toSeq

  protected val index = new DelegatingGlobalSymbolIndex()
  protected val workspace = new TestingWorkspaceSearch

  protected lazy val presentationCompiler: PresentationCompiler = {
    val javaLibrary = PackageIndex.javaLibrary // todo

    val myclasspath: Seq[Path] = javaLibrary

    JdkSources().foreach(jdk => index.addSourceJar(jdk, dialects.Scala213))
    println("CLASSPATH: " + myclasspath)
    val search = new TestingSymbolSearch(
      ClasspathSearch
        .fromClasspath(myclasspath, ExcludedPackagesHandler.default),
      new Docstrings(index),
      workspace,
      index,
    )

    JavaPresentationCompiler()
      .withSearch(search)
      .withConfiguration(
        PresentationCompilerConfigImpl()
          .copy(isHoverDocumentationEnabled = documentationHoverEnabled)
      )
  }

  val tmp: AbsolutePath = AbsolutePath(Files.createTempDirectory("java.metals"))

  def params(code: String, filename: String = "test.java"): (String, Int) = {
    val code2 = code.replace("@@", "")
    val offset = code.indexOf("@@")
    if (offset < 0) {
      fail("missing @@")
    }

    inspectDialect(filename, code2)
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
      case (po, _, _) =>
        inspectDialect(filename, code2)
        (code2, po, po)
    }
  }

  private def inspectDialect(filename: String, code2: String) = {
    val file = tmp.resolve(filename)
    Files.write(file.toNIO, code2.getBytes(StandardCharsets.UTF_8))
    val dialect = dialects.Scala213
    try index.addSourceFile(file, Some(tmp), dialect)
    catch {
      case NonFatal(e) =>
        println(s"warn: ${e.getMessage()}")
    }
    println("ADDED FILE")
    println("FILE: " + file)
    workspace.inputs(filename) = (code2, dialect)
  }
}
