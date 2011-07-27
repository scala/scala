// This was fixed in r25277 but is enough different
// from the case I was knowingly fixing, I'm throwing it
// in there.
object HasArgs {
  def boop(params: List[List[_]]) = params match {
    case List(List()) => 2
  }
}
