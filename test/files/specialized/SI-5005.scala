import scala.tools.partest._
import java.io._

object Test extends DirectTest {
  
  override def extraSettings: String = "-usejavacp -Xprint:spec -optimize -Ylog:inliner -d " + testOutput.path

  override def code = """
    class C2[@specialized(Boolean) U]() {
      @inline final def apply(x: U): U = x
    }

    class B {
      (new C2[Boolean]())(true)
    }
  """

  override def show(): Unit = {
    // redirect err to out, for inliner log
    System.setErr(new PrintStream(System.out));
    compile()
  }
}
