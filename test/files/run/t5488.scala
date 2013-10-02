class A0[@specialized(Int, AnyRef) A]()
class B0[@specialized(Int, AnyRef) A, @specialized(Int, AnyRef) B]()
class C0[@specialized(Int, AnyRef) A, @specialized(Int, AnyRef) B, @specialized(Int, AnyRef) C]()

object Test {
  def main(args:Array[String]) {
    def show(x: Any) = println(x.getClass.getName)

    show(new A0[Int]())
    show(new A0[AnyRef]())

    show(new B0[Int, Int]())
    show(new B0[Int, AnyRef]())
    show(new B0[AnyRef, Int]())
    show(new B0[AnyRef, AnyRef]())

    show(new C0[Int, Int, Int]())
    show(new C0[Int, Int, AnyRef]())
    show(new C0[Int, AnyRef, Int]())
    show(new C0[Int, AnyRef, AnyRef]())
    show(new C0[AnyRef, Int, Int]())
    show(new C0[AnyRef, Int, AnyRef]())
    show(new C0[AnyRef, AnyRef, Int]())
    show(new C0[AnyRef, AnyRef, AnyRef]())
  }
}
