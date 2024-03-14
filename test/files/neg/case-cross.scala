//> using options -Xsource:3

// warn about case class synthetic method getting access modifier from constructor

case class C private (c: Int) {
  def copy(c: Int) = this // warn about apply instead
}
