import scala.tools.partest._

// Test that static methods in Java interfaces (new in Java 8)
// are callable from jointly compiler Scala code.
object Test extends CompilerTest {
  import global._

  override lazy val units: List[CompilationUnit] = {
    // This test itself does not depend on JDK8.
    javaCompilationUnits(global)(staticMethodInInterface) ++
    compilationUnits(global)(scalaClient)
  }

  private def staticMethodInInterface = """
public interface Interface {
  public static int staticMethod() {
    return 42;
  }
}

  """

  private def scalaClient = """
object Test {
  val x: Int = Interface.staticMethod()
}

class C extends Interface // expect no errors about unimplemented members.

  """

  // We're only checking we can compile it.
  def check(source: String, unit: global.CompilationUnit): Unit = ()
}
