class Test {
  trait PropTraverser {
    def apply(x: Int): Unit = {}
  }

  def gather(x: Int) {
    (new PropTraverser {})(x)
  }
}
