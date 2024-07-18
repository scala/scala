//> using options -Yrangepos
class Test {
  trait PropTraverser {
    def apply(x: Int): Unit = {}
  }

  def gather(x: Int): Unit = {
    (new PropTraverser {})(x)
  }
}
