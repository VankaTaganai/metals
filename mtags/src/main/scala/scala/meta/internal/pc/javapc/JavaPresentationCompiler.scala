package scala.meta.internal.pc.javapc

import org.eclipse.lsp4j.{
  CompletionItem,
  CompletionList,
  Diagnostic,
  DocumentHighlight,
  Hover,
  SelectionRange,
  SignatureHelp,
  TextEdit
}

import java.net.URI
import java.nio.file.Path
import java.util.Optional
import java.{lang, util}
import java.util.concurrent.{
  CompletableFuture,
  ExecutorService,
  ScheduledExecutorService
}
import scala.meta.pc.{
  AutoImportsResult,
  DefinitionResult,
  OffsetParams,
  PresentationCompiler,
  PresentationCompilerConfig,
  RangeParams,
  SymbolSearch,
  VirtualFileParams
}

class JavaPresentationCompiler extends PresentationCompiler {

  override def complete(
      params: OffsetParams
  ): CompletableFuture[CompletionList] =
    CompletableFuture.completedFuture(
      new JavaCompletionProvider(params).completions()
    )

  override def completionItemResolve(
      item: CompletionItem,
      symbol: String
  ): CompletableFuture[CompletionItem] = ???

  override def signatureHelp(
      params: OffsetParams
  ): CompletableFuture[SignatureHelp] = ???

  override def hover(params: OffsetParams): CompletableFuture[Optional[Hover]] =
    ???

  override def rename(
      params: OffsetParams,
      name: String
  ): CompletableFuture[util.List[TextEdit]] = ???

  override def definition(
      params: OffsetParams
  ): CompletableFuture[DefinitionResult] = ???

  override def typeDefinition(
      params: OffsetParams
  ): CompletableFuture[DefinitionResult] = ???

  override def documentHighlight(
      params: OffsetParams
  ): CompletableFuture[util.List[DocumentHighlight]] = ???

  override def getTasty(
      targetUri: URI,
      isHttpEnabled: Boolean
  ): CompletableFuture[String] = ???

  override def autoImports(
      name: String,
      params: OffsetParams,
      isExtension: lang.Boolean
  ): CompletableFuture[util.List[AutoImportsResult]] = ???

  override def implementAbstractMembers(
      params: OffsetParams
  ): CompletableFuture[util.List[TextEdit]] = ???

  override def insertInferredType(
      params: OffsetParams
  ): CompletableFuture[util.List[TextEdit]] = ???

  override def extractMethod(
      range: RangeParams,
      extractionPos: OffsetParams
  ): CompletableFuture[util.List[TextEdit]] = ???

  override def convertToNamedArguments(
      params: OffsetParams,
      argIndices: util.List[Integer]
  ): CompletableFuture[util.List[TextEdit]] = ???

  override def didChange(
      params: VirtualFileParams
  ): CompletableFuture[util.List[Diagnostic]] = ???

  override def didClose(uri: URI): Unit = ???

  override def semanticdbTextDocument(
      filename: URI,
      code: String
  ): CompletableFuture[Array[Byte]] = ???

  override def selectionRange(
      params: util.List[OffsetParams]
  ): CompletableFuture[util.List[SelectionRange]] = ???

  override def shutdown(): Unit = ???

  override def restart(): Unit = ???

  override def withSearch(search: SymbolSearch): PresentationCompiler = ???

  override def withExecutorService(
      executorService: ExecutorService
  ): PresentationCompiler = ???

  override def withScheduledExecutorService(
      scheduledExecutorService: ScheduledExecutorService
  ): PresentationCompiler = ???

  override def withConfiguration(
      config: PresentationCompilerConfig
  ): PresentationCompiler = ???

  override def withWorkspace(workspace: Path): PresentationCompiler = ???

  override def newInstance(
      buildTargetIdentifier: String,
      classpath: util.List[Path],
      options: util.List[String]
  ): PresentationCompiler = ???

  override def diagnosticsForDebuggingPurposes(): util.List[String] = ???

  override def isLoaded: Boolean = ???

  override def scalaVersion(): String = ???
}
