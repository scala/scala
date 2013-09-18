class B[@specialized(Int, AnyRef, Unit) A, @specialized(Int, AnyRef, Unit) B](f: A => B)
class C[@specialized(Int, AnyRef) A, @specialized(Int, AnyRef) B, @specialized(Int, AnyRef) C](f: (A, B) => C)

object Test {
  def main(args:Array[String]) {
    def show(x: Any) = println(x.getClass.getName)

    show(new B((x: Int) => 1))
    show(new B((x: Int) => "abc"))
    show(new B((x: Int) => ()))
    show(new B((x: AnyRef) => 1))
    show(new B((x: AnyRef) => "abc"))
    show(new B((x: AnyRef) => ()))
    show(new B((x: Unit) => 1))
    show(new B((x: Unit) => "abc"))
    show(new B((x: Unit) => ()))

    show(new C((x: Int, y: Int) => 1))
    show(new C((x: Int, y: Int) => "abc"))
    show(new C((x: Int, y: AnyRef) => 1))
    show(new C((x: Int, y: AnyRef) => "abc"))
    show(new C((x: AnyRef, y: Int) => 1))
    show(new C((x: AnyRef, y: Int) => "abc"))
    show(new C((x: AnyRef, y: AnyRef) => 1))
    show(new C((x: AnyRef, y: AnyRef) => "abc"))
  }
}
