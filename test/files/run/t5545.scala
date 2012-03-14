import scala.tools.partest._
import java.io._

object Test extends DirectTest {

  override def extraSettings: String = "-usejavacp -d " + testOutput.path + " -cp " + testOutput.path

  override def code = """
    // SI-5545
    trait F[@specialized(Int) T1, R] {
      def f(v1: T1): R
      def g = v1 => f(v1)
    }
  """.trim

  override def show(): Unit = {
    // redirect err to out, for logging
    val prevErr = System.err
    System.setErr(System.out)
    compile()
    // the bug manifests at the second compilation, when the bytecode is already there
    compile()
    System.setErr(prevErr)
  }

  override def isDebug = false // so we don't get the newSettings warning
}
