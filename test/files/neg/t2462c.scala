
import annotation._

@implicitNotFound("No C of ${ A }")
class C[A]

trait X$Y
/* using the $$ separator for expanded names is unwise
trait X$$Y
trait X$$$Y
trait X$$$$Y
 */

trait Foo[A]

class Test {
  def f[A: C] = ???
  f[X$Y]
/* using the $$ separator for expanded names is unwise
  f[X$$Y]
  f[X$$$Y]
  f[X$$$$Y]
 */
  f[Foo[Int]]
}
