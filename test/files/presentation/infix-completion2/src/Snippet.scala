object Snippet{val x: One.type = 123; One + x./*!*/}
object One {
  def +(other: One) = this
  def youCompleteMe(other: One.type) = ()
}
