class A(val options: Seq[String])

object Test {
  implicit def ss: Equiv[Seq[String]] = error("dummy")
  implicit def equivA(implicit seqEq: Equiv[Seq[String]]): Equiv[A] = error("dummy")
  implicitly[Equiv[A]]
}