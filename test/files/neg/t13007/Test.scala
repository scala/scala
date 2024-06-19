package s

trait T { self: j.J =>
  override def i(): Boolean = true
  def t1 = i()
  def t2 = this.i()
  def t3 = self.i()

  def t4 = j()
  def t5 = this.j()
  def t6 = self.j()

  def t7 = k()
  def t8 = this.k()
  def t9 = self.k()
}

class C extends j.J with T
