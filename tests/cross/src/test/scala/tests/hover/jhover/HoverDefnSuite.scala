package tests.hover.jhover

import tests.pc.javapc.BaseJavaHoverSuite

class HoverDefnSuite extends BaseJavaHoverSuite {
  check(
    "int value",
    """
      |class A {
      |    public static int NUMBER = 42;
      |
      |    public static void main(String args[]){
      |        NU@@MBER;
      |    }
      |}
      |""".stripMargin,
    """
      |int
      |""".stripMargin.javaHover,
  )

  check(
    "test.clazz",
    """
      |import java.util.List;
      |
      |class A {
      |  public static void main(String args[]){
      |    List<Integer> x = List.of(1);
      |    @@x;
      |  }
      |}
      |""".stripMargin,
    """|java.util.List<java.lang.Integer>
       |""".stripMargin.javaHover,
  )

  check(
    "var assigment",
    """
      |import java.util.List;
      |
      |class A {
      |  public static void main(String args[]){
      |    var @@x = List.of(1);
      |  }
      |}
      |""".stripMargin,
    """|java.util.List<java.lang.Integer>
       |""".stripMargin.javaHover,
  )

//  check(
//    "fail",
//    """
//      |import java.util.List;
//      |
//      |class A {
//      |  public static void main(String args[]){
//      |    %<%List<Integer> x = new ArrayList<>()%>%;
//      |  }
//      |}
//      |""".stripMargin,
//    "".javaHover,
//  )

  check(
    "kek",
    """
      |import java.util.List;
      |
      |class @@A {
      |  public static void main(String args[]){
      |  }
      |}
      |""".stripMargin,
    "kek.A".javaHover,
  )
}
