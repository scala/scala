package scala.swing

import event.Event

class Reactions {
  type Reaction = PartialFunction[Event, unit]

  private var parts: List[Reaction] = List()

  def += (r: Reaction) = { parts = r :: parts }

  def -= (r: Reaction) = {
    def withoutR(xs: List[Reaction]): List[Reaction] =
      if (xs.isEmpty) xs
      else if (xs.head == r) xs.tail
      else xs.head :: withoutR(xs.tail)
    parts = withoutR(parts)
  }

  def send(e: Event) = {
    def sendTo(ps: List[Reaction]): Unit = ps match {
      case Nil =>
      case p :: ps =>
        if (p isDefinedAt e) p(e)
        /*else*/ sendTo(ps)
    }
    sendTo(parts)
  }
}
