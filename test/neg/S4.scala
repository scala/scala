/* This one compiles, but even if we would have dependent
** constructor types, it would be not sound.
*/
class S4(a: Other) extends a.Inner() {
    def foo(x: a.Inner) = x;
    val b = new Other();
    foo(new S4(b))
}
class Other() {
    class Inner() {}
}
