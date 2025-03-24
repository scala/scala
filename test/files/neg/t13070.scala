//> using options -Wunused:patvars -Werror
class C {
  def g = {
    val t = Option((27, 42))
    for {
      ns <- t
      (i, j) = ns // warn // warn
    } yield 42
  }
}
class D {
  def g = {
    val t = Option((27, 42))
    for {
      ns <- t
      (i, j) = ns // warn
    } yield 42 + i
  }
}

// the following do not warn under -Wunused:patvars in Scala 2 (but Scala 3 does)
object `pat vardef are patvars` {
  private var (i, j) = (42, 27) // warn // warn
}

object `patvar is assignable` {
  var (i, j) = (42, 27) // no warn nonprivate
  j += 1
  println((i, j))
}

object `privy patvar is assignable` {
  private var (i, j) = (42, 27) // warn
  j += 1
  println((i, j))
}

object `local patvar is assignable` {
  def f() = {
    var (i, j) = (42, 27) // warn
    j += 1
    println((i, j))
  }
}

object `mutable patvar in for` {
  def f(xs: List[Int]) = {
    for (x <- xs; y = x + 1 if y > 10)
    yield {
      var z :: Nil = y :: Nil: @unchecked // warn
      z + 10
    }
  }
}

class `unset var requires -Wunused` {
  private var i = 0 // no warn as we didn't ask for it
  def f = println(i)
}
