//> using options -Ylog:superaccessors -Ydebug
object X {
  def m: Int = {
    trait C {
      protected[C] def f: Int
    }
    object C {
      class C2 extends C {
        protected[C] def f: Int = 42  // no, limitation
        def test = f
      }
    }
    new C.C2().test
  }
}
object Y {
  def n: Int = {
    trait C {
      protected[C] def f: Int
    }
    class X { private def x = 17 }
    locally {
      object X {
        val y = 27
      }
      object C {
        class C2 extends C {
          protected[C] def f: Int = 42   // no
          def test = f + X.y
        }
      }
      new C.C2().test
    }
  }
}

// other combinations
// mangling qualified privates says:
// Expanded 'g' to 'Base$$g' in trait Base
trait Base {
  protected[Base] def f: Int
  private[Base] def g: Int
  private[Base] def h: Int
  private[Base] def p: Int
}
object Base {
  class Child extends Base {
    override protected[Base] def f: Int = 42   // ok, companion
    // was: overrides nothing (because of name mangling)
    override private[Base] def g: Int = 42     // ok, companion
    override protected[Base] def h: Int = 42   // ok, private[C] widens to protected[C]
    override protected def p: Int = 42         // error, protected only overrides protected
  }
}
