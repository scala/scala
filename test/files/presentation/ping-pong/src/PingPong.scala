
class Ping {

  val pong = new Pong(this)

  def name = "ping"

  def loop: Unit =/*?*/ { poke() }

  def poke: Unit =/*?*/ { pong./*!*/poke() }

  override def toString = name
}

class Pong(ping: Ping) {

  val name/*?*/ = "pong"

  def poke(): Unit = { ping./*!*/poke() }

  override def toString = name
}
