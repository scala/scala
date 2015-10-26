
trait t9509 {
  val foo_bar = "OK"
  val foo＿bar = "fullwidth"

  // is Java but not UnicodeIdentifierStart
  //val ₵ = "cents"

  def f = s"$foo_bar"
  def g = s"$foo＿bar"
}
