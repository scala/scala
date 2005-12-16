
class Foo(x: Int) {}
case class Bar(y: Int) extends Foo(y);


trait T {}
trait U {}
class C() {}


trait T1;
trait T2 {}
trait T5 extends T;
trait T6 extends T {}
trait T7 extends T with U;
trait T8 extends T with U {}

class C1();
class C2() {}
class C5() extends C();
class C6() extends C() {}
class C7() extends C() with U;
class C8() extends C() with U {}

case class D1();
case class D2() {}
case class D5() extends C();
case class D6() extends C() {}
case class D7() extends C() with U;
case class D8() extends C() with U {}

object M1;
object M2 {}
object M5 extends C();
object M6 extends C() {}
object M7 extends C() with U;
object M8 extends C() with U {}



