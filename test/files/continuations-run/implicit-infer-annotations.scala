import annotation._

object A {
  class foo[-B,+C] extends StaticAnnotation with TypeConstraint

  def shift[A, B, C](fun: (A => B) => C): A @foo[B, C] = ???
  def reset[A, C](ctx: => (A @foo[A, C])): C = ???

  def m1 = reset { shift { f: (Int => Range) => f(5) }.to(10) }
}

object B {
  import scala.util.continuations._

  def m1 = reset { shift { f: (Int => Range) => f(5) }.to(10) }
  def m2 = reset { val a = shift { f: (Int => Range) => f(5) } ; a.to(10) }

  val x1 = reset{
    shift{ cont: (Int => Range) =>
      cont(5)
    }.to(10)
  }

  val x2 = reset{
    val a = shift{ cont: (Int => Range) =>
      cont(5)
    }
    a.to(10)
  } // x is now Range(5, 6, 7, 8, 9, 10)

  val x3 = reset{
    shift{ cont: (Int => Int) =>
      cont(5)
    } + 10
  } // x is now 15

  val x4 = reset{
    10 :: shift{ cont: (List[Int] => List[Int]) =>
      cont(List(1, 2, 3))
    }
  } // x is List(10, 1, 2, 3)

  val x5 = reset{
    new scala.runtime.RichInt(shift{ cont: (Int => Range) =>
      cont(5)
    }) to 10
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    import B._
    println(x1)
    println(x2)
    println(x3)
    println(x4)
    println(x5)
  }
}
