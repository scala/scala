
class Ping {

  val pong = new Pong(this)

  def name = "ping"

  def loop/*?*/ { poke() }

  def poke/*?*/ { pong./*!*/poke() }

  override def toString = name
}

class Pong(ping: Ping) {

  val name/*?*/ = "pong"

  def poke() { ping./*!*/poke() }

  override def toString = name
}