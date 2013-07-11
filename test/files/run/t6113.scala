import scala.language.higherKinds

trait Foo[C[_]]

object Test extends App {
  import scala.reflect.runtime.universe._
  println(typeOf[Foo[({type l[X] = (Int, X)})#l]])
}
