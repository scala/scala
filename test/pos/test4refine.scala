trait C {}
trait D {}
trait E {}

module test {
  def c: C = c;
  def d: D = d;
  def e: E = e;
}

import test._;

trait S extends o.I {
    type Y = D;
    def bar: E = foo(c,d);
}

abstract class O() {
    type X;
    abstract trait I {
	type Y;
        def foo(x: X, y: Y): E = e;
    }
    val i:I with { type Y = E } = null;
    val j:I with { type Y = X } = null;
}

module o extends O() {
  type X = C;

  def main = {
    val s: S = null;
    import s._;
    foo(c,d);
    o.i.foo(c,e);
    o.j.foo(c,c);
    bar
  }
}

class Main() {
  val s: S = null;
  import s._;
  foo(c,d);
  o.i.foo(c,e);
  o.j.foo(c,c);
  bar;
}

