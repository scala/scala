package test;
trait test {
  type T;
  trait Manager {
    type T <: test.this.T;
    def foo(t : T) = {};
  }
  object M0 extends Manager {
    override type T = test.this.T;
    override def foo(t : T) = super.foo(t);
  }
  def t : T;
  M0.foo(t);  
}
