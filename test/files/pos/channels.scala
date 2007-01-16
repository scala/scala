class Channel[a]

import collection.mutable.Set

case class ![a](chan: Channel[a], data: a)

/*
object Bang {
  def unapply[a](x: ![a]): Option[{Channel[a], a}] =
    Some(x.chan, x.data)
}

*/
object Test extends Application {
  object IC extends Channel[int]
  def f[b](x: ![b]): int = x match {
    case send: ![c] =>
      send.chan match {
        case IC => send.data
      }
  }
}

object Test2 extends Application {
  object IC extends Channel[Set[int]]
  def f[b](s: ![b]): Set[int] = s match {
    case IC ! x => x
  }
}

