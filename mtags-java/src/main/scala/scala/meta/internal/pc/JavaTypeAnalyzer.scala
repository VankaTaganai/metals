package scala.meta.internal.pc

import com.sun.source.util.{JavacTask, TreePath, Trees}

import javax.lang.model.`type`.TypeMirror

class JavaTypeAnalyzer(task: JavacTask) {
  def typeMirror(path: TreePath): TypeMirror =
    Trees.instance(task).getTypeMirror(path)
}
