object Test extends Application {
  class A
  class B extends A
  def foo(x: A, y: B) = print(1)
  val foo = new {
    // def apply(x: B, y: A) = print(3)
    def apply = (x: B, z: B) => print(4)
  }

  foo(new B, new B)
}

// This code prints 1. If we remove comment, then it will print 4.
// Moreover following code prints 3 (which is most strange thing): 

object Test2 extends Application {
  class A
  class B extends A
  def foo(x: A, y: B) = print(1)
  val foo = new {
    def apply(x: B, y: A) = print(3)
    def apply = new {
      def apply = (x: B, z: B) => print(4)
    }
  }

  foo(new B, new B)
}