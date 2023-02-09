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
import scala.collection.Seq
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
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
import scala.jdk.CollectionConverters._
import scala.meta.internal.mtags.BuildInfo
import scala.meta.internal.pc.{
  DefinitionResultImpl,
  EmptySymbolSearch,
  PresentationCompilerConfigImpl
}

case class JavaPresentationCompiler(
    buildTargetIdentifier: String = "",
    classpath: Seq[Path] = Nil,
    options: List[String] = Nil,
    search: SymbolSearch = EmptySymbolSearch,
    ec: ExecutionContextExecutor = ExecutionContext.global,
    sh: Option[ScheduledExecutorService] = None,
    config: PresentationCompilerConfig = PresentationCompilerConfigImpl(),
    workspace: Option[Path] = None
) extends PresentationCompiler {

  private val javaCompiler = new JavaMetalsGlobal(search, config)

  override def complete(
      params: OffsetParams
  ): CompletableFuture[CompletionList] =
    CompletableFuture.completedFuture( // fixme
      new JavaCompletionProvider(javaCompiler, params).completions()
    )

  override def completionItemResolve(
      item: CompletionItem,
      symbol: String
  ): CompletableFuture[CompletionItem] = CompletableFuture.completedFuture(item)

  override def signatureHelp(
      params: OffsetParams
  ): CompletableFuture[SignatureHelp] =
    CompletableFuture.completedFuture(new SignatureHelp())

  override def hover(params: OffsetParams): CompletableFuture[Optional[Hover]] =
    CompletableFuture.completedFuture(
      Optional.ofNullable(
        new JavaHoverProvider(javaCompiler, params).hover().orNull
      )
    )

  override def rename(
      params: OffsetParams,
      name: String
  ): CompletableFuture[util.List[TextEdit]] =
    CompletableFuture.completedFuture(Nil.asJava)

  override def definition(
      params: OffsetParams
  ): CompletableFuture[DefinitionResult] =
    CompletableFuture.completedFuture(DefinitionResultImpl.empty)

  override def typeDefinition(
      params: OffsetParams
  ): CompletableFuture[DefinitionResult] =
    CompletableFuture.completedFuture(DefinitionResultImpl.empty)

  override def documentHighlight(
      params: OffsetParams
  ): CompletableFuture[util.List[DocumentHighlight]] =
    CompletableFuture.completedFuture(Nil.asJava)

  override def getTasty(
      targetUri: URI,
      isHttpEnabled: Boolean
  ): CompletableFuture[String] = CompletableFuture.completedFuture("")

  override def autoImports(
      name: String,
      params: OffsetParams,
      isExtension: lang.Boolean
  ): CompletableFuture[util.List[AutoImportsResult]] =
    CompletableFuture.completedFuture(
      new JavaAutoImportsProvider(javaCompiler, params).autoImports().asJava
    )

  override def implementAbstractMembers(
      params: OffsetParams
  ): CompletableFuture[util.List[TextEdit]] =
    CompletableFuture.completedFuture(Nil.asJava)

  override def insertInferredType(
      params: OffsetParams
  ): CompletableFuture[util.List[TextEdit]] =
    CompletableFuture.completedFuture(Nil.asJava)

  override def extractMethod(
      range: RangeParams,
      extractionPos: OffsetParams
  ): CompletableFuture[util.List[TextEdit]] =
    CompletableFuture.completedFuture(Nil.asJava)

  override def convertToNamedArguments(
      params: OffsetParams,
      argIndices: util.List[Integer]
  ): CompletableFuture[util.List[TextEdit]] =
    CompletableFuture.completedFuture(Nil.asJava)

  override def didChange(
      params: VirtualFileParams
  ): CompletableFuture[util.List[Diagnostic]] =
    CompletableFuture.completedFuture(Nil.asJava)

  override def didClose(uri: URI): Unit = ()

  override def semanticdbTextDocument(
      filename: URI,
      code: String
  ): CompletableFuture[Array[Byte]] =
    CompletableFuture.completedFuture(Array.emptyByteArray)

  override def selectionRange(
      params: util.List[OffsetParams]
  ): CompletableFuture[util.List[SelectionRange]] =
    CompletableFuture.completedFuture(Nil.asJava)

  override def shutdown(): Unit = ()

  override def restart(): Unit = ()

  override def withSearch(search: SymbolSearch): PresentationCompiler =
    copy(search = search)

  override def withExecutorService(
      executorService: ExecutorService
  ): PresentationCompiler =
    copy(ec = ExecutionContext.fromExecutorService(executorService))

  override def withScheduledExecutorService(
      scheduledExecutorService: ScheduledExecutorService
  ): PresentationCompiler = copy(sh = Some(scheduledExecutorService))

  override def withConfiguration(
      config: PresentationCompilerConfig
  ): PresentationCompiler = copy(config = config)

  override def withWorkspace(workspace: Path): PresentationCompiler =
    copy(workspace = Some(workspace))

  override def newInstance(
      buildTargetIdentifier: String,
      classpath: util.List[Path],
      options: util.List[String]
  ): PresentationCompiler =
    copy(
      buildTargetIdentifier = buildTargetIdentifier,
      classpath = classpath.asScala,
      options = options.asScala.toList
    )

  override def diagnosticsForDebuggingPurposes(): util.List[String] = Nil.asJava

  override def isLoaded: Boolean = true

  override def scalaVersion(): String = BuildInfo.scalaCompilerVersion
}
