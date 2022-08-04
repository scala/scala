trait A {
  def m(a:Int): Int
}

trait B extends A {
  override def m(a:Int): Int = { return a; }
}

trait C extends A {
  abstract override def m(a:Int):Int = { return super.m(a); }
}

trait D extends B with C {
  override def m(a:Int):Int = { return super.m(a); }
}

trait E extends C with B {
  abstract override def m(a:Int):Int = { return super.m(a); }
}

class X extends E with D