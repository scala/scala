
package t6278

import language.implicitConversions

object test {
  def ok(): Unit = {
    class Foo(val i: Int) {
      def foo[A](body: =>A): A = body
    }
    implicit def toFoo(i: Int): Foo = new Foo(i)

    val k = 1
    k foo println("k?")
    val j = 2
  }
  def nope(): Unit = {
    implicit class Foo(val i: Int) {
      def foo[A](body: =>A): A = body
    }

    val k = 1
    k foo println("k?")
    //lazy
    val j = 2
  }
  def main(args: Array[String]): Unit = {
    ok(); nope()
  }
}
