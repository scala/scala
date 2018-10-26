trait F[T1, R] { def apply(funArg: T1): R }

trait SetOps[A, +C] extends F[A, Boolean] { final def apply(setEl: A): Boolean = false }

class AbstractSet[A] extends SetOps[A, AbstractSet[A]]
class AbstractSet2[A] extends AbstractSet[A] with SetOps[A, AbstractSet2[A]]

object Test {
  def main(args: Array[String]): Unit = {
    new AbstractSet2[String]
  }
}
