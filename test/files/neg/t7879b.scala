//> using options -Xsource:3 -Xsource-features:case-copy-by-name
case class C(i: Int)(js: Int*) { def sum = i + js.sum }

class Usage {
  def f(c: C): C = c.copy(42)(Seq(9, 9, 9): _*)
}
