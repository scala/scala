//> using options -Xsource:3
//

class Test {
  def foo(xs: Int*): Seq[Int] = xs

  val s: Seq[Int] = Seq(1, 2, 3)
  foo(s*)
  foo((s ++ s)*)

  // not very useful, but supported by Scala 3 (and matches what works with `: _*` syntax)
  foo(
    s*,
  )

  s match {
    case Seq(elems*) => println(elems)
  }

  s match {
    case Seq(x, rest*) => println(rest)
  }

  // regression tests for comparison
  s match {
    case Seq(elems @ _*) => println(elems)
  }

  s match {
    case Seq(x, rest @ _*) => println(rest)
  }

  // more parens
  s match {
    case Seq((xs) @ _*) => xs
  }

  /* also disallowed in Scala 3
  s match {
    case Seq((xs)*) => xs
  }
  */
}
