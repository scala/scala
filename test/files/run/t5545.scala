import scala.tools.partest._
import java.io._

object Test extends DirectTest {

  override def extraSettings: String = s"-usejavacp -cp ${testOutput.path}"

  override def code = s"""
    // scala/bug#5545
    trait F[@specialized(Int) T1, R] {
      def f(v1: T1): R
      def g = v1 => f(v1)
    }
  """.trim

  override def show(): Unit = {
    compile()
    // the bug manifests at the second compilation, when the bytecode is already there
    compile()
  }
}
