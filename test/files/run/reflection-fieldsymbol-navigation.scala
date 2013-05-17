import scala.reflect.runtime.universe._

class C {
  var x = 2
}

object Test extends App {
  val x = typeOf[C].member(TermName("x")).asTerm
  println(x)
  println(x.isVar)
  println(x.accessed)
  println(x.accessed.asTerm.isVar)
  println(x.getter)
  println(x.setter)
}