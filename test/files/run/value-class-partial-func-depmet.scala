class C
class A { class C }

object Test {
  def main(args: Array[String]) {
    val a = new A

    new VC("").foo(a)
  }
}

class VC(val a: Any) extends AnyVal {
   def foo(a: A) = {
     val pf: PartialFunction[a.C, Any] = { case x => x }
     (pf: PartialFunction[Null, Any]).isDefinedAt(null)
   }
}

// 2.11.0-M6
// test/files/run/value-class-partial-func-depmet.scala:14: error: overriding method applyOrElse in trait PartialFunction of type [A1 <: a.C, B1 >: Any](x: A1, default: A1 => B1)B1;
//  method applyOrElse has incompatible type
//      val pf: PartialFunction[a.C, Any] = { case x => x }
//                                          ^
// one error found
