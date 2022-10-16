
package foo {

  trait Baz[A]
  object Baz {
    implicit def instance[A]: Baz[A] = ???
  }

  package syntax {
    object all {
      implicit def ops1[A: Baz](a: A): BarOps1 = new BarOps1(a)
      implicit def ops2[A: Baz](a: A): BarOps2 = new BarOps2(a)
    }

    class BarOps1(val a: Any) extends AnyVal {
      def bar(x: Int): String = ???
    }

    class BarOps2(val a: Any) extends AnyVal {
      private[syntax] def bar(x: Int): String = ???
    }
  }
}

import foo.syntax.all._

object Main {
  def main(args: Array[String]): Unit = {
    val a = new Object
    a.bar(42)
  }
}
