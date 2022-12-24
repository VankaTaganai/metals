package scala.meta.internal.pc.javapc

import com.sun.source.tree.PackageTree
import com.sun.source.util.JavacTask
import org.eclipse.lsp4j.TextEdit

import scala.meta.internal.pc.{AutoImportPosition, AutoImportsResultImpl}
import scala.meta.pc.{AutoImportsResult, OffsetParams}
import org.eclipse.{lsp4j => l}

import scala.jdk.CollectionConverters.SeqHasAsJava
import scala.reflect.internal.util.Position
import scala.tools.nsc.MainBench.theCompiler.newSourceFile

final class JavaAutoImportsProvider(
    compiler: JavaMetalsGlobal,
    params: OffsetParams
) {
  def autoImports(): List[AutoImportsResult] = {
    val task: JavacTask = compiler.compilationTask(params.text())
    val scanner = compiler.scanner(task)
    val node = compiler.compilerTreeNode(scanner, params.offset())

    val importPosition = autoImportPosition(params, scanner)

    val start = inferIdentStart(params.offset(), params.text())
    val end = inferIdentEnd(params.offset(), params.text())

    val identifier = params.text().substring(start, end)

    val autoImports =
      compiler.allQualifiedNames().filter(_.endsWith(s".$identifier")).map {
        qName =>
          val pos = importPosition.get

          val editPosition = Position
            .offset(
              newSourceFile(params.text(), params.uri().toString),
              pos.offset
            )
            .toLsp

          AutoImportsResultImpl(
            qName,
            (new TextEdit(editPosition, s"import $qName") :: Nil).asJava
          )
      }

    println("identifier: " + identifier)

    autoImports
  }

  def autoImportPosition(
      params: OffsetParams,
      scanner: JavaTreeScanner
  ): Option[AutoImportPosition] = {
    for {
      pkg <- scanner.lastVisitedParentTrees.collectFirst {
        case pkg: PackageTree => pkg
      }
    } yield {
      new AutoImportPosition(
        scanner.getEndPosition(pkg).toInt,
        params.text(),
        true
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

  implicit class XtensionPositionMetals(pos: Position) {
    // Same as `Position.includes` except handles an off-by-one bug when other.point > pos.end
    def metalsIncludes(other: Position): Boolean = {
      pos.includes(other) &&
      (!other.isOffset || other.point != pos.end)
    }
    private def toPos(offset: Int): l.Position = {
      val line = pos.source.offsetToLine(offset)
      val column = offset - pos.source.lineToOffset(line)
      new l.Position(line, column)
    }

    def isAfter(other: Position): Boolean = {
      pos.isDefined &&
      other.isDefined &&
      pos.point > other.point
    }

    def toLsp: l.Range = {
      if (pos.isRange) {
        new l.Range(toPos(pos.start), toPos(pos.end))
      } else {
        val p = toPos(pos.point)
        new l.Range(p, p)
      }
    }
  }
}
