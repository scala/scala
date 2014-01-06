object ScalaForRubyists {
  class RichInt(n: Int) {
    def days = 1000*60*60*24*n
  }

  implicit def RichInt(n: Int): RichInt = new RichInt(n)

  val x = 10.days
  // a couple parser corner cases I wanted not to break
  val y = 5.0e0 + 5e7
}
