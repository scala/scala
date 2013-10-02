
import scala.language.{ reflectiveCalls }

object Test {
  val c = { class C; new C { def foo = 1 } }
  val a = { class B { def bar = 5 }; class C extends B; new C }
  val e = { class A; class B extends A; classOf[B] }

  def main(args: Array[String]): Unit = {
    println(c.foo)
    println(a.bar)
    println(e)
  }
}
