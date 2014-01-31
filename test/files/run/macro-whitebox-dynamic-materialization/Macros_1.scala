import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

trait Foo[T]

class C1(val x: Int)
class C2(val x: String)

trait LowPriority {
  implicit def lessSpecific[T]: Foo[T] = null
}

object Foo extends LowPriority {
  implicit def moreSpecific[T]: Foo[T] = macro Macros.impl[T]
}

object Macros {
  def impl[T: c.WeakTypeTag](c: Context) = {
    import c.universe._
    val tpe = weakTypeOf[T]
    if (tpe.members.exists(_.info =:= typeOf[Int]))
      c.abort(c.enclosingPosition, "I don't like classes that contain integers")
    q"new Foo[$tpe]{ override def toString = ${tpe.toString} }"
  }
}