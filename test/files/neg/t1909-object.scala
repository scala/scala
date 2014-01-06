class Kaboom(a: Any) {
  def this() = {
    this({
      object InnerTrouble
      InnerTrouble
    })
  }
}

object Test extends App {
  new Kaboom()
}