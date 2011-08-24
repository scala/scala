class Ticket1909 (x: Int) {
  var z = 12
  def this() = this({
    def bar() = this.z + 5
    bar
  })
}