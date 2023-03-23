package scala.meta.internal.pc

import javax.lang.model.`type`.TypeMirror
import javax.lang.model.element.{ExecutableElement, VariableElement}
import scala.jdk.CollectionConverters.CollectionHasAsScala
import javax.lang.model.element.ElementKind.CONSTRUCTOR

object JavaLabels {
  def typeLabel(t: TypeMirror): String =
    t.accept(new JavaTypeVisitor(), null)

  def argumentLabel(v: VariableElement): String = {
    val argType = typeLabel(v.asType())
    val argName = v.getSimpleName

    s"$argType $argName"
  }

  def argumentsLabel(e: ExecutableElement): String =
    e.getParameters.asScala.map(argumentLabel).mkString(", ")

  def executableName(e: ExecutableElement): String =
    (if (e.getKind == CONSTRUCTOR)
       e.getEnclosingElement.getSimpleName
     else e.getSimpleName).toString

  def executableLabel(e: ExecutableElement): String =
    s"${executableName(e)}(${argumentsLabel(e)})"

}
