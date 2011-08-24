class Stuff {
  def zoop(p: Any{def &(q: Int): Int}) = p & 7
  def floop = new { def & = "Hello" }

  assert((floop &) == "Hello")
  assert(zoop(10) == 2)
}

object Test extends App {
  new Stuff
}
