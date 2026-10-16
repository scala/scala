//> using options -Werror
//> using jvm 16+

import java.time.LocalDate
import scala.runtime.BoxedUnit

class C {
  def literal = 451.synchronized {} // warn

  def boxed(x: Integer) = x.synchronized {} // warn

  def unit(u: BoxedUnit) = u.synchronized {} // warn

  def localDate(d: LocalDate) = d.synchronized {} // warn

  def optional(o: java.util.Optional[String]) = o.synchronized {} // warn

  def ok(s: String) = s.synchronized {} // no warn
}
