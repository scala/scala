/* I believe this code is correct, but the compiler rejects it:
**
** S8.scala:18: type mismatch;
** found   : M.x.A
** required: M.x.a.B
**        val y: x.a.B = new x.A(); //correct?
**                              ^
** For a given value x of type S8, type x.A should be
** a subtype of x.a.B.
*/
class S8() {
    val a: S8 = this;
    class A() extends a.B() {}
    class B() {}
}
object M {
    val x = new S8();
    val y: x.a.B = new x.A(); //correct?
}
