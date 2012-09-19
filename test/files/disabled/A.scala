trait As {
  trait C extends D {
    override def foo = this  /// Shouldn't cause the change
    override def foo(act: List[D]) = this
  }
    
  abstract class D{
    def foo: D = this
    def foo(act: List[D]) = this
  }
}
