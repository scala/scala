import scala.language.experimental.macros

class CState(val x: Int) extends AnyVal

class C private (state: CState) {
  def this(x: Int) = macro Macros.ctor
  def x = state.x
  override def toString = s"C($x)"
}

object C {
  def apply(x: Int) = {
    println(s"intercepted new C($x)")
    new C(new CState(x))
  }
}

object Test extends App {
  println(new C(2))
}
