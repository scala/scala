class Channel[a]

import collection.mutable.Set

case class ![a](chan: Channel[a], data: a)

/*
object Bang {
  def unapply[a](x: ![a]): Option[{Channel[a], a}] = 
    Some(x.chan, x.data)
}

*/
object Test extends App {
  object IC extends Channel[Int]
  def f[b](x: ![b]): Int = x match {
    case send: ![c] => 
      send.chan match {
        case IC => send.data
      }
  }
}

object Test2 extends App {
  object IC extends Channel[Set[Int]]
  def f[b](s: ![b]): Set[Int] = s match {
    case IC ! x => x
  }
}
 
