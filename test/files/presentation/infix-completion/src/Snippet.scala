object Snippet{val x = 123; One + One./*!*/}
object One {
  def +(other: One) = this
  def youCompleteMe(other: One.type) = ()
}
