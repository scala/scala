package tastytest

object TestDelayedFancyColours {
  def foo(p: DelayedFancyColours.Pretty) = p match {
    case DelayedFancyColours.FancyColours.Colour.Violet => ()
  }
}
