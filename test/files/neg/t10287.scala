//> using options -Werror -Wunused:_ -Xsource:3

class C {
  def unused_patvar =
    Some(42) match { case x => 27 } // warn
  def a =
    for {
      x <- Some(42) // ok
    } yield (x + 1)
  def b =
    for {
      x <- Some(42) // warn
    } yield 27
  def c =
    for {
      x <- Some(42) // warn
      y <- Some(27) // ok
    } yield y
  def d =
    for {
      x <- Some(42) // ok
      y <- Some(27) // warn
    } yield x
  def e =
    for {
      x <- Some(42) // ok
      y = 3 // warn
    } yield x
  def f =
    for {
      x <- Some(42) // warn
      y = 3 // ok
    } yield y
  def g =
    for {
      x <- Some(1 -> 2) // ok
      y = 3 // warn
      (a, b) = x // ok
    } yield (a + b)
  def h(xs: List[Int]) =
    for {
      x <- xs
      y = x * 2
      _ = println(x)
    } yield y
}

case class K[A](a: A, i: Int)

class Fixes {
  def leavenstain(s: String, t: String) = {
    val n = s.length
    val m = t.length
    for (i <- 1 to n; s_i = s(i - 1); j <- 1 to m) {
      println("" + s_i + t(j - 1))
    }
  }

  def f[A](ks: List[K[A]]): List[K[?]] = {
    for {
      K((s: String), j) <- ks
      x = j*2
    } yield K(s*2, x)
  }
}
