trait A {
  type T;
  trait X {
    private[A] type S <: T;
    /*private[A]*/ def foo : S;
  }
  trait Y extends X {
    override private[A] type S = Any;
    override /*private[A]*/ def foo = null;
  }
}

