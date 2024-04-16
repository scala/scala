//> using options -Xsource:3

class D {
  def *(y: Int): Int = y
  def unrelated(y: Int): Int = y
}

object nope {
  val d = new D
  import d.{* => huh}
  import d.{_ => also_no}
}
