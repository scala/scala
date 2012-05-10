trait Bounds[@specialized A] {
  // okay without `>: A`
  def x[B >: A]: Unit = new Bounds[B] {
    lazy val it = ???  // def or val okay
    it
  }
}