class Outer {
  object X
  val xx: X.type = X

  class Inner {
    assert(X.isInstanceOf[X.type])
    assert(!new Outer().X.isInstanceOf[X.type])
    assert(Outer.this.isInstanceOf[Outer.this.type])
    assert(!new Outer().isInstanceOf[Outer.this.type])

    class DoubleInner {
      assert(X.isInstanceOf[X.type])
      assert(!new Outer().X.isInstanceOf[X.type])
      assert(Outer.this.isInstanceOf[Outer.this.type])
      assert(!new Outer().isInstanceOf[Outer.this.type])
    }
    object O2 {
      assert(X.isInstanceOf[X.type])
      assert(!new Outer().X.isInstanceOf[X.type])
      assert(Outer.this.isInstanceOf[Outer.this.type])
      assert(!new Outer().isInstanceOf[Outer.this.type])
    }
  }

  object InnerObj {
    assert(X.isInstanceOf[X.type])
    assert(!new Outer().X.isInstanceOf[X.type])
    assert(Outer.this.isInstanceOf[Outer.this.type])
    assert(!new Outer().isInstanceOf[Outer.this.type])
 
    trait DoubleInner {
      assert(X.isInstanceOf[X.type])
      assert(!new Outer().X.isInstanceOf[X.type])
      assert(Outer.this.isInstanceOf[Outer.this.type])
      assert(!new Outer().isInstanceOf[Outer.this.type])
    }
  }

  final class InnerFinal {
    assert(xx.isInstanceOf[xx.type])
    assert(!new Outer().X.isInstanceOf[xx.type])
    assert(Outer.this.isInstanceOf[Outer.this.type])
    assert(!new Outer().isInstanceOf[Outer.this.type])
  }
}

object Test extends App {
  val outer = new Outer
  val outerInner = new outer.Inner
  new outerInner.DoubleInner
  outerInner.O2
  outer.InnerObj
  new outer.InnerObj.DoubleInner {}
  new outer.InnerFinal
}
