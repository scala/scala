// scalac: -Xsource:3

class D {
  def *(y: Int): Int = y
  def unrelated(y: Int): Int = y
}

// TODO
object nope {
  val d = new D
  import d.{* => huh}
  import d.{_ => also_no}
  `should fail`()
}

// OK
object rename {
  val d = new D
  import d.{unrelated => f, *}
  def x = f(42)
  def y = *(27)
}
