trait C {}
trait D {}
trait E {}

object test {
  def c: C = c;
  def d: D = d;
  def e: E = e;
}

import test._;

trait S extends o.I[D] {
    def bar: E = foo(c,d);
}

class O[X]() {
    trait I[Y] {
        def foo(x: X, y: Y): E = e;
    }
    val i:I[E] = null;
    val j:I[X] = null;
}

object o extends O[C]() {

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

