import scala.tools.partest._

// Simple sanity test for -Yshow-member-pos.
object Test extends DirectTest {
  override def extraSettings: String = "-usejavacp -Ystop-after:parser -Yshow-member-pos \"\" -d " + testOutput.path
  override def show() = compile()
  override def code = """
class A(val a: Int = 1) {

}

object A {
  def bippy = {
    def hello = 55
    "" + hello
  }
  class Dingo {
    def foooooz = /****





    ****/ {



      val a = 1


      a
    }
  }
}

class B { def f = 1 }

"""
}
