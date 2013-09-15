class t3832 {
  def this(un: Int) = {
    this()
    def bippy = this
    ()
  }
  def this(un: Boolean) = {
    this()
    def boppy = () => this
    ()
  }
}

object Test extends App {
  new t3832(0)
  new t3832(true)
}
