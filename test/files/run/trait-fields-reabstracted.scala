// A class that mixes in a trait `val` and redeclares it abstractly must still implement the trait
// setter: the trait's initializer calls it. Before the fix, the setter was dropped (no field to
// assign) and `new C*` threw AbstractMethodError.
trait T { val m: String = "T" }

abstract class B extends T { def m: String }
class C extends B with T { override val m = "C" }

abstract class B1 extends T { def m: String }
class C1 extends B1 { override val m = "C1" }

abstract class B2 extends T { val m: String }
class C2 extends B2 { override val m = "C2" }

object Test {
  def main(args: Array[String]): Unit = {
    println(new C().m)
    println(new C1().m)
    println(new C2().m)
  }
}
