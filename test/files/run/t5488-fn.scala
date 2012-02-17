class B[@specialized(Int, AnyRef, Unit) A, @specialized(Int, Unit) B](f: A => B)
class C[@specialized(Int, AnyRef) A, @specialized(Int, AnyRef) B, @specialized(Int) C](f: (A, B) => C)
// Not yet:
// class B[@specialized(Int, AnyRef, Unit) A, @specialized(Int, AnyRef, Unit) B](f: A => B)
// class C[@specialized(Int, AnyRef) A, @specialized(Int, AnyRef) B, @specialized(Int, AnyRef) C](f: (A, B) => C)

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
/**  If the return types are specialized on AnyRef as well:

files/run/t5488-fn.scala:18: error: type mismatch;
 found   : Unit => String
 required: Unit => B$sp
    show(new B((x: Unit) => "abc"))
         ^
files/run/t5488-fn.scala:24: error: type mismatch;
 found   : (Int, Object) => String
 required: (Int, B$sp) => C$sp
    show(new C((x: Int, y: AnyRef) => "abc"))
         ^
files/run/t5488-fn.scala:26: error: type mismatch;
 found   : (Object, Int) => String
 required: (A$sp, Int) => C$sp
    show(new C((x: AnyRef, y: Int) => "abc"))
         ^
three errors found
**/
