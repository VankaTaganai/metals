package tests.pc.javapc

class AutoImportsSuite extends BaseJavaAutoImportsSuite {
  check(
    "List import",
    """
      |package tests.java;
      |
      |class Simple{
      |    public static void main(String args[]){
      |        <<List>>.of(1, 2, 3);
      |    }
      |}
      |""".stripMargin,
    """
      |java.util.List
      |""".stripMargin,
  )

  check(
    "Map import",
    """
      |package tests.java;
      |
      |class Simple{
      |    public static void main(String args[]){
      |        <<Map>><Int, String> a;
      |    }
      |}
      |""".stripMargin,
    """
      |java.util.Map
      |""".stripMargin,
  )
}
