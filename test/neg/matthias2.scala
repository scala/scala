class A() {
    val x: A = this;
    val y: x.type = x;
    type T = y.type;
}
abstract class B() extends A() {
    override val y: T;
}
