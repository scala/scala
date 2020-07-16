trait Inner {
  def f(): Outer
}

class Outer(o: Set[Inner]) {
  def this() = this(Set(1).map{
    case k => new Inner{
      def f(): Outer = Outer.this
    }
  })
}
