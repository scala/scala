import scala.annotation.tailrec

// putting @tailrec through the paces
object Main {
  @tailrec
  def facfail(n: Int): Int =
    if (n == 0) 1
    else n * facfail(n - 1)

  @tailrec
  def facsucc(n: Int, acc: Int): Int =
    if (n == 0) acc
    else facsucc(n - 1, n * acc)

  @tailrec def loopy1(x: Int): Int = loopy1(x - 1)

  def ding {
    object dong {
      @tailrec def loopy2(x: Int): Int = loopy2(x)
    }
    ()
  }

  def inner(q: Int) = {
    @tailrec
    def loopy3(x: Int): Int = loopy3(x + 1)

    loopy3(q)
  }
}

class Bob {
  // these should work
  @tailrec private def succ1(x: Int): Int = succ1(x)
  @tailrec final def succ2(x: Int): Int = succ2(x)
  @tailrec final def succ3[T](in: List[T], acc: List[T]): List[T] = in match {
    case Nil      => Nil
    case x :: xs  => succ3(xs, x :: acc)
  }

  // not private, not final
  @tailrec def fail1(x: Int): Int = fail1(x)

  // a typical between-chair-and-keyboard error
  @tailrec def fail2[T](xs: List[T]): List[T] = xs match {
    case Nil      => Nil
    case x :: xs  => x :: fail2(xs)
  }

  object innerBob {
    @tailrec def succ4(x: Int): Int = succ4(x)
  }
}
