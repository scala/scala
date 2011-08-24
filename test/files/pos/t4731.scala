import java.util.Comparator

trait Trait1[T] { def foo(arg: Comparator[T]): Unit }

trait Trait2[T] extends Trait1[T] { def foo(arg: Comparator[String]): Int = 0 }

class Class1 extends Trait2[String] { }

object Test {
  def main(args: Array[String]): Unit = {
    val c = new Class1
    c.foo(Ordering[String])
  }
}
