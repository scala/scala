/* Another example for a non-terminating compiler run.
*/
class S7[T]() {
    val a: S7[T] = this;
    class A() extends a.C() {}
    class C() extends a.A() {}
}
