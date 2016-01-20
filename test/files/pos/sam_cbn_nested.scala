class CBNNested {
  def cbn(x: => Int) = ???
  def nestedCbn: Unit = {
    def nested(i: Int): Unit = cbn(i)
    println()
  }

  def wrapped = cbn {
    val ps = 1 // owner structure messed up as its lifted into the sam body?
    def foo = ps
    foo
  }

}