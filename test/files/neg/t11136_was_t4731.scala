import java.util.Comparator

/* This accidentally started as a pos/ test, with the initial fix addressing a crasher in the backend.
The real problem is that `foo`'s return type is not refined covariantly, so the override should be ruled out.
Refchecks was too eager in pruning the paits it considers, so this was never detected.
*/
trait Trait1[T] { def foo(arg: Comparator[T]): Unit }

trait Trait2[T] extends Trait1[T] { def foo(arg: Comparator[String]): Int = 0 }

class Class1 extends Trait2[String] { }

object Test {
  def main(args: Array[String]): Unit = {
    val c = new Class1
    c.foo(Ordering[String])
  }
}
