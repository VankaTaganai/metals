package tests.hover.jhover

import tests.pc.javapc.BaseJavaHoverSuite

class HoverDocSuite extends BaseJavaHoverSuite {
  check(
    "int value",
    """
      |class A {
      |    public static void main(String args[]){
      |        java.util.Collections.<Integer>empty@@List();
      |    }
      |}
      |""".stripMargin,
    """
      |int
      |""".stripMargin.javaHover,
  )
}
