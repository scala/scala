class Test {
  def repeat(x: Int, y: String) = y * x
  val sayAaah = repeat(_, "a") // partial eta-expansion recovers fun param types from method (place holder syntax)
  val thrice = x => repeat(3, x) // partial eta-expansion recovers fun param types from method (explicit version)
  val repeatFlip = (x, y) => repeat(y, x) // partial eta-expansion recovers fun param types from method (explicit version, two params)
}
