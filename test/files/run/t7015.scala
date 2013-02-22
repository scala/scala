object Test {
  def main(args : Array[String]) : Unit = {
    println(s"Method returns Null type: $f")
    println(s"Method takes non Null type: ${g(null)}")

    // pass things through the g function because it expects
    // a string. If we haven't adapted properly then we'll
    // get verify errors
    val b = new B
    println(s"call through method ${g(b.f(null))}")
    println(s"call through bridge ${g((b: A).f(null))}")

    println(s"fetch field: ${g(b.nullField)}")
    println(s"fetch field on companion: ${g(B.nullCompanionField)}")

    val x = f
    println(s"fetch local: ${g(x)}")

    val nulls = Array(f, f, f)
    println(s"fetch array element: ${g(nulls(0))}")

    println(s"method that takes object: ${q(f)}")
    println(s"method that takes anyref: ${r(f)}")
    println(s"method that takes any: ${s(f)}")
  }

  def f: Null = null

  def g(x: String) = x

  def q(x: java.lang.Object) = x
  def r(x: AnyRef) = x
  def s(x: Any) = x
}

abstract class A {
	def f(x: String): String
}

class B extends A {
	val nullField = null

	// this forces a bridge method because the return type is different
	override def f(x: String) : Null = null
}

object B {
	val nullCompanionField = null
}