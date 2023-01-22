package tests.hover.jhover

import tests.pc.javapc.BaseJavaHoverSuite

class HoverTermSuite extends BaseJavaHoverSuite {
  check(
    "new class",
    """
      |class A {
      |
      |    private A(String str, int num) {};
      |
      |    public static void main(String args[]){
      |        new @@A("str", 42);
      |    }
      |}
      |""".stripMargin,
    """
      |private void A(java.lang.String str, int num)
      |""".stripMargin.javaHover,
  )

  check(
    "new T class",
    """
      |class A<T> {
      |
      |    private A(String str, T t) {};
      |
      |    public static void main(String args[]){
      |        new <Integer>@@A("str", 42);
      |    }
      |}
      |""".stripMargin,
    """
      |private void A(java.lang.String str, T t)
      |""".stripMargin.javaHover,
  )

//  check(
//    "new class anon",
//    """
//      |class A {
//      |
//      |    private A(String str, int num) {};
//      |
//      |    public static void main(String args[]){
//      |        new @@A("str", 42) {
//      |            final int x = 2;
//      |        }
//      |    }
//      |}
//      |""".stripMargin,
//    """
//      |private void A(java.lang.String str, T t)
//      |""".stripMargin.javaHover,
//  )
  check(
    "import1",
    """
      |import java.n@@io.file.*;
      |""".stripMargin,
    """|```java
       |package java.nio
       |```
       |""".stripMargin,
  )

  check(
    "import2",
    """
      |import jav@@a.nio.file.*;
      |""".stripMargin,
    """|```java
       |package java
       |```
       |""".stripMargin,
  )

  check(
    "import3",
    """
      |import java.nio.fil@@e.*;
      |""".stripMargin,
    """|```java
       |package java.nio.file
       |```
       |""".stripMargin,
  )

  check(
    "widen",
    """
      |class A {
      |    public static void main(String args[]){
      |        System.out.println(java.nio.file.FileVisitResult.CONTIN@@UE);
      |    }
      |}
      |""".stripMargin,
    """public static final java.nio.file.FileVisitResult CONTINUE""".javaHover,
  )
}
