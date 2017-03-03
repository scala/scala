trait T1 { def a: Any }

trait T2 extends T1 { object a; object b; private object c; def usec: Any = c}
trait T3 extends T2

class C1 extends T1 { object a; object b }
class C2 extends C1
class C3 extends T2
class C4 extends T3

object Test {
  def main(args: Array[String]): Unit = {
    val (c1, c2, c3, c4) = (new C1, new C2, new C3, new C4)
    c1.a; c1.b; (c1: T1).a
    c2.a; c2.b; (c2: T1).a
    c3.a; c3.b; (c3: T1).a; c3.usec
    c4.a; c4.b; (c4: T1).a; c4.usec
  }

}
