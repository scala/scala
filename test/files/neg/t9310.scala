import scala.language.dynamics

trait Test {
  class C extends Dynamic {
    def applyDynamic(arg: Any): C = ???
  }
  val c = new C
  def f = c.m(42)
}

/*
object Test {
  case class LeonAny(v: Any) extends Dynamic {
    def applyDynamic(args: Any*): LeonAny = this
  }

  implicit def fooToFoo(a: Any): LeonAny = {
    LeonAny(a)
  }

  def test() {
    val a: LeonAny = 42
    val b: LeonAny = Nil

    b += a
  }
}
*/
