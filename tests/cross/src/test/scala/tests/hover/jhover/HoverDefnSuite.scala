package tests.hover.jhover

import tests.pc.javapc.BaseJavaHoverSuite

class HoverDefnSuite extends BaseJavaHoverSuite {
  check(
    "int value",
    """
      |class Simple{
      |    public static int NUMBER = 42;
      |
      |    public static void main(String args[]){
      |        <<NU@@MBER>>;
      |    }
      |}
      |""".stripMargin,
    """
      |int
      |""".stripMargin.javaHover,
  )
}
