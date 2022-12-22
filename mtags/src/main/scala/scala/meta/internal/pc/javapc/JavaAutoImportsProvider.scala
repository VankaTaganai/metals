package scala.meta.internal.pc.javapc

import com.sun.source.tree.PackageTree
import com.sun.source.util.JavacTask

import scala.meta.internal.pc.AutoImportPosition
import scala.meta.pc.{AutoImportsResult, OffsetParams}

final class JavaAutoImportsProvider(
    compiler: JavaMetalsGlobal,
    params: OffsetParams
) {
  def autoImports(): List[AutoImportsResult] = {
    val task: JavacTask = compiler.compilationTask(params.text())
    val scanner = compiler.scanner(task)
    val node = compiler.compilerTreeNode(scanner, params.offset())

    val importPosition = autoImportPosition(params, scanner)

    Nil
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
}
