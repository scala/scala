// test `with` keyword in for comprehensions

object Test {
  case class Fut[+A](value: Option[A], name: String) {
    println(s"creating $this")
    def map[B](f: A => B): Fut[B] =
      Fut(value.map(f), s"mapped $name")
    def withFilter(p: A => Boolean): Fut[A] =
      Fut(value.filter(p), s"filtered $name")
    def product[B](other: Fut[B]): Fut[(A, B)] =
      Fut((for (v <- value; ov <- other.value) yield (v, ov)), s"$name with ${other.name}")
    def flatMap[B](f: A => Fut[B]): Fut[B] = {
      println(s"flatMapping $this")
      value.fold(Fut[B](None, name))(f)
    }
    def foreach(f: A => Unit): Unit = {
      println(s"foreach $this")
      value.foreach(f)
    }
    override def toString = name + value.fold("")(" having " + _)
  }
  object Fut {
    def apply[A](a: A, name: String): Fut[A] = Fut(Option(a), name)
  }

  {
    for {
      a <- Fut(1, "a")
      a2 = a * 2
      a3 = a * 3
      b <- Fut(2, "b") with
      c <- Fut(3, "c") with
      d <- Fut(4, "d") if d > a && d > b && d > c
      d2 = b + c + d
      e <- Fut(5, "e") with
      f <- Fut(6, "f")
    } yield a + b + c + d + e + f
  }

  println()

  {
    for {
      a <- Fut(1, "a")
      a2 = a * 2
      a3 = a * 3
      b <- Fut(2, "b") with
      c <- Fut(3, "c") with
      d <- Fut(4, "d") if d > a && d > b && d > c
      d2 = b + c + d
      e <- Fut(5, "e") with
      f <- Fut(6, "f")
    } {
      println("finished")
    }
  }

  def main(args: Array[String]) {}
}
