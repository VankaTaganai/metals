package scala.meta.internal.pc

import com.sun.source.tree.PackageTree
import com.sun.source.util.JavacTask
import org.eclipse.lsp4j.TextEdit

import scala.meta.internal.pc.{AutoImportPosition, AutoImportsResultImpl}
import scala.meta.pc.{AutoImportsResult, OffsetParams}
import org.eclipse.{lsp4j => l}

import scala.jdk.CollectionConverters.SeqHasAsJava

final class JavaAutoImportsProvider(
    compiler: JavaMetalsGlobal,
    params: OffsetParams,
) {
  def autoImports(): List[AutoImportsResult] = {
    val task: JavacTask = compiler.compilationTask(params.text())
    val scanner = compiler.scanner(task)
    val position =
      CursorPosition(params.offset(), params.offset(), params.offset())
    val node = compiler.compilerTreeNode(scanner, position)

    val importPosition = autoImportPosition(params, scanner)

    val start = inferIdentStart(params.offset(), params.text())
    val end = inferIdentEnd(params.offset(), params.text())

    val identifier = params.text().substring(start, end)

    val autoImports =
      compiler.allQualifiedNames().filter(_.endsWith(s".$identifier")).map {
        qName =>
          val pos = importPosition.get

          val editPosition = params.toLsp

          AutoImportsResultImpl(
            qName,
            (new TextEdit(editPosition, s"import $qName") :: Nil).asJava,
          )
      }

    autoImports
  }

  def autoImportPosition(
      params: OffsetParams,
      scanner: JavaTreeScanner,
  ): Option[AutoImportPosition] = {
    for {
      pkg <- scanner.lastVisitedPackageTrees.headOption
    } yield {
      new AutoImportPosition(
        scanner.getEndPosition(pkg).toInt,
        params.text(),
        true,
      )
    }
  }

  private def inferIdentStart(pos: Int, text: String): Int = {
    var i = pos - 1
    while (i >= 0 && Character.isJavaIdentifierPart(text.charAt(i))) {
      i -= 1
    }
    i + 1
  }

  def inferIdentEnd(pos: Int, text: String): Int = {
    var i = pos
    while (i < text.length && Character.isJavaIdentifierPart(text.charAt(i))) {
      i += 1
    }
    i
  }

  implicit class XtensionPositionMetals(pos: OffsetParams) {
    private def toPos(offset: Int): l.Position = {
      val lines = pos.text().split("\n")

      val (line, sum, _) = lines.foldLeft((0, 0, false)) {
        case ((a, sum, true), _) => (a, sum, true)
        case ((a, sum, _), b) =>
          if (sum + b.length < pos.offset()) (a + 1, sum + b.length, false)
          else (a, sum, true)
      }
      val column = offset - sum
      new l.Position(line, column)
    }

    def toLsp: l.Range = {
//      if (pos.isRange) {
//        new l.Range(toPos(pos.start), toPos(pos.end))
//      } else {
      val p = toPos(pos.offset())
      new l.Range(p, p)
//      }
    }
  }
}
