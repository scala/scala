import scala.reflect.runtime.universe._

class C {
  var x = 2
}

object Test extends App {
  val x = typeOf[C].member(newTermName("x")).asTerm
  println(x)
  println(x.isVariable)
  println(x.accessed)
  println(x.accessed.asTerm.isVariable)
  println(x.getter)
  println(x.setter)
}