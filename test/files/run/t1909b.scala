class Ticket1909 (x: Int) {
  def this() = this({
    def bar() = 5
    bar
  })
}
object Test extends App {
  new Ticket1909()
}
