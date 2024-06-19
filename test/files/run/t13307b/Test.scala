package s {

  trait T extends j.J {
    override def i(): Boolean = true

    def t1 = i()
    def t2 = this.i()

    // def t4 = j()
    // def t5 = this.j()

    def t7 = k()
    def t8 = this.k()
  }

  class C extends j.J with T
}

object Test {
  def main(args: Array[String]): Unit = {
    val c = new s.C
    assert(c.t1)
    assert(c.t2)
    assert(!c.t7)
    assert(!c.t8)
  }
}
