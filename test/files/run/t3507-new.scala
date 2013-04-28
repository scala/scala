
import scala.language.{ existentials }
import scala.reflect.runtime.universe._

class A {
  object b {
    object c
  }
  def m = b.c
}

object Test extends App {
  var a: A = new A // mutable
  val c /*: object _1.b.c forSome { val _1: A } */ = a.m // widening using existential

  def mani[T: TypeTag](x: T) = println(typeOf[T])
  mani/*[object _1.b.c]*/(c) // kaboom in manifestOfType / TreeGen.mkAttributedQualifier
  // --> _1 is not in scope here
}
