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
 
// channels.scala:17:
//     case send: ![c] =>
//                ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  ![b]
//         pt  ![b]
//      pattp  ![c]
//   pattp+pt  ![c]
//   pt+pattp  ![b]
//     result  ![b]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 =:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt =:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp ~:= result
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }