package test.generic {
  class C1[A] {
    def m1(): Unit = ()
  }

  trait T1[A] extends C1[A] {
    def t1(x: A) = x
  }

  class C2[A] extends T1[A]
}

import scala.tools.partest._

object Bug4891 extends SigTest {
  import test.generic._

  def main(args: Array[String]): Unit = {
    show[T1[_]]()
    show[C1[_]]()
    show[C2[_]]("m1")

    println(classOf[T1[_]].getGenericSuperclass)
    classOf[T1[_]].getGenericInterfaces foreach println
  }
}
