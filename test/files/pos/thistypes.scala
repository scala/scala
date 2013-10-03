trait B {
  trait I {}
  def foo: B.this.I;
}

trait C extends B {
  def foo: C.this.I;
}
