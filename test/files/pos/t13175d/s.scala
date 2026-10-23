//> using jvm 17+
package p

trait Tag[T]
trait Other

// typing the match while completing `S` needs the children of `Shape`, which needs the parents of `S`
object M { val v = (null: Shape) match { case _: Other => 0; case _ => 1 } }
final class S extends Shape with Tag[M.v.type]
