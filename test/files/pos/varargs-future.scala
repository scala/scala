// scalac: -Xsource:3
//

class Test {
  def foo(xs: Int*): Seq[Int] = xs

  val s: Seq[Int] = Seq(1, 2, 3)
  foo(s*)

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
}
