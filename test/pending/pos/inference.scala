import scala.reflect.runtime.universe._

// inference illuminator
object Test {
  class D1[T1 : TypeTag, T2 <: T1 : TypeTag](x: T1) { println(typeOf[(T1, T2)]) }
  class D2[T1 : TypeTag, T2 >: T1 : TypeTag](x: T1) { println(typeOf[(T1, T2)]) }
  class D3[+T1 : TypeTag, T2 <: T1 : TypeTag](x: T1) { println(typeOf[(T1, T2)]) }
  class D4[-T1 : TypeTag, T2 >: T1 : TypeTag](x: T1) { println(typeOf[(T1, T2)]) }

  class E1[T1 : TypeTag, T2 <: T1 : TypeTag](x: D1[T1, T2]) { println(typeOf[(T1, T2)]) }
  class E2[T1 : TypeTag, T2 >: T1 : TypeTag](x: D2[T1, T2]) { println(typeOf[(T1, T2)]) }
  class E3[+T1 : TypeTag, T2 <: T1 : TypeTag](x: D3[T1, T2]) { println(typeOf[(T1, T2)]) }
  class E4[-T1 : TypeTag, T2 >: T1 : TypeTag](x: D4[T1, T2]) { println(typeOf[(T1, T2)]) }

  def main(args: Array[String]): Unit = {
    // WHY YOU NO LIKE NOTHING SO MUCH SCALAC?
    val d1 = new D1(5)
    val d2 = new D2(5)
    val d3 = new D3(5)
    val d4 = new D4(5)

    new E1(d1)    // fails
    new E2(d2)
    new E3(d3)    // fails
    new E4(d4)
  }
  //  found   : Test.D1[Int,Nothing]
  //  required: Test.D1[Int,T2]
  // Note: Nothing <: T2, but class D1 is invariant in type T2.
  // You may wish to define T2 as +T2 instead. (SLS 4.5)
  //     new E1(d1)
  //            ^
  // test/pending/pos/inference.scala:22: error: type mismatch;
  //  found   : Test.D3[Int,Nothing]
  //  required: Test.D3[Int,T2]
  // Note: Nothing <: T2, but class D3 is invariant in type T2.
  // You may wish to define T2 as +T2 instead. (SLS 4.5)
  //     new E3(d3)
  //            ^
  // two errors found
}