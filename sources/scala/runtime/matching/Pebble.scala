package scala.runtime.matching ;

case class Pebble(vx:Int,p:Address) with Ordered[Pebble] {

  def compareTo[ b >: Pebble <% Ordered[b] ]( y:b ) = y match {
    case Pebble( vx2, p2 ) =>
      if( vx == vx2 )
        p compareTo p2
      else
        vx compareTo vx2
    case _ => -( y compareTo this )
  }

}
