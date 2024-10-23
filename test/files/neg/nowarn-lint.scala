//> using options -Werror -Xlint:unit-special,unused

import annotation.nowarn

// exercise the lint warning
class C[@specialized(Unit) A](a: A)
class D[@specialized(Specializable.Primitives) A](a: A)

// feel confident that remaining examples will either suppress warning
// or warn no suppression
@nowarn("cat=lint-unit-specialization")
class E[@specialized(Unit) A](a: A)
@nowarn("cat=lint-unit-specialization")
class F[@specialized(Specializable.Primitives) A](a: A)

// other positions
@nowarn("cat=lint-unit-specialization")
class G[@specialized(Unit) A](x: Int, a: A)

// what if it's a member with forwarders?
// was: incorrectly warned that nowarn was unused.
@nowarn("cat=lint-unit-specialization")
class H[@specialized(Unit) A](x: AnyRef, val a: A)

@nowarn("cat=lint-unit-specialization")
class HH[@specialized(Unit) A](val a: A)(val aa: A)

// actual unused thing
class J {
  private val j = 42
  def f: Unit = ()
}

// canonically unused nowarn
@nowarn("any")
trait U

/* as reported
import scala.util.control.ControlThrowable

@nowarn("cat=lint-unit-specialization")
class NonLocalReturnControl[@specialized(Byte, Short, Int, Long, Char, Float, Double, Boolean, Unit) T](val key: AnyRef, val value: T) extends ControlThrowable {
  final override def fillInStackTrace(): Throwable = this
}
*/
