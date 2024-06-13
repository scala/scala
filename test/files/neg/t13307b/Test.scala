package s

trait T extends j.J {
  override def i(): Boolean = true
  def t1 = i()
  def t2 = this.i()

  def t4 = j()
  def t5 = this.j()

  def t7 = k()
  def t8 = this.k()
}

class C extends j.J with T
