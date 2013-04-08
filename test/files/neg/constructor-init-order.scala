trait Foo0 {
  val quux1: String
  val quux2 = quux1  // warning here is "future work"
}

class Foo1 extends Foo0 {
  val bar1         = baz     // warn
  val bar2         = lazybaz // no warn
  val bar3         = defbaz  // no warn
  val baz          = "oops"
  lazy val lazybaz = "ok"
  def defbaz       = "ok"
  val quux1        = "oops"
}

class Foo2 {
  var bar1         = baz     // warn
  var bar2         = lazybaz // no warn
  var bar3         = defbaz  // no warn
  var baz          = "oops"
  lazy val lazybaz = "ok"
  def defbaz       = "ok"
}
