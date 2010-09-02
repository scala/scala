object Test extends Application {
  class A
  class B extends A
  def foo(x: A, y: B) = print(1)
  val foo = new {
    //def apply(x: B, y: A) = print(3)
    def apply = (x: B, z: B) => print(4)
  }

  foo(new B, new B)
}
