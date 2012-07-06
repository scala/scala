
import reflect.runtime.{universe=>u}
import scala.reflect.runtime.{currentMirror => cm}

class C {
   def foo[T: u.TypeTag](x: String) = 1
   def foo[T: u.TypeTag, S: u.TypeTag](x: String) = 2
}

object Test extends App {
  val c = new C
  val im = cm.reflect(c)
  val foo = u.typeOf[C] member u.newTermName("foo") asTermSymbol
  val f1 = foo.resolveOverloaded(
      targs = Seq(u.typeOf[Int]),
      posVargs = Seq(u.typeOf[String])
  )

  val f2 = foo.resolveOverloaded(
      targs = Seq(u.typeOf[Int],
        u.typeOf[Int]), posVargs = Seq(u.typeOf[String])
  )

  val m1 = im.reflectMethod(f1 asMethodSymbol)
  val m2 = im.reflectMethod(f2 asMethodSymbol)

  assert(m1("a", u.typeTag[Int]) == c.foo[Int]("a"))
  assert(m2("a", u.typeTag[Int], u.typeTag[Int]) == c.foo[Int, Int]("a"))
}
