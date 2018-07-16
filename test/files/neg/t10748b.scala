
private[p] class C(i: Int = 42) { def c = new C() }

private[p] class D(i: Int = 42) { def f = new Q }
class X { def d = new D() }
