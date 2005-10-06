trait C {}

class O[X]() {
    trait I[Y] {
        def foo(y: Y): Y = y;
    }
    val j:I[X] = null;
}

object o extends O[C]() {
  def c: C = c;
  def main = {
    o.j.foo(c);
  }
}

