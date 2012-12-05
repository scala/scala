trait U {
	trait U1 {
		class X
	}
	type U11 <: U1
  val u : U11 = null.asInstanceOf[U11]
}
trait A extends U

trait B extends U {
  def foo = ""
  class U11 extends U1 { class X extends super.X { foo } } // refer to foo to add $outer pointer
  override val u = new U11
}
class C {
  val ab: A with B = new A with B // `B with A` works.

  def foo {
    // fails
    new ab.u.X

    // works:
    val u = ab.u
    new u.X
  }
}
object Test {
	def main(args: Array[String]) {
    // java.lang.NoSuchMethodError: A.u()LB$U11;
    // at C.foo(t6677b.scala:23)
		new C().foo
	}
}
