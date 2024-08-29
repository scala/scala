//> using options -Xfatal-warnings -deprecation

class Cls
class SubCls extends Cls

final class FinCls

class Eqs {
  override def equals(o: Any): Boolean = super.equals(o)
}

case class CC(x: Int)
class SubCC(x: Int) extends CC(x) { override def equals(o: Any) = true }

class Hello2024 {

  // operations: ==, !=, eq, ne
  //   - todo: cover `eq/ne` extensively in this test
  //   - no "non-sensible" warnings when using equals

  // inventory

  val obj = new AnyRef
  val any: Any = obj

  val cls = new Cls
  val sub = new SubCls
  val fin = new FinCls

  val eqs = new Eqs

  val cc = CC(1)
  val scc: CC = new SubCC(1)

  val str = "kno"
  val opt = Option(1)
  val lst = List(1)
  val set = Set(1)
  val map = collection.mutable.Map(1 -> 1)

  // primitives: 1, true, ()
  val int = 1
  val boo = true
  val uni = ()
  // null

  val intB = Integer.valueOf(1)
  val booB = java.lang.Boolean.TRUE
  val uniB = scala.runtime.BoxedUnit.UNIT

  // fresh: `new Cls`, () => 1


  // obj
  locally {
    // obj == any, references: doesn't warn, no need to test
    obj == 1 // n
    obj equals 1 // n
    obj == () // n
    obj == intB // n
    obj == new Cls // n: no warn here, obj can be anything
    obj == (() => 1) // n
  }

  // any: same as obj. additional warning for "bypasses cooperative equality" when using equals
  // instead of ==. not extensively tested here, this test is about a different warning.
  locally {
    any == 1 // n
    any equals 1 // w: bypass
  }

  // warning for unrelated classes ("will most likely never compare equal")
  locally {
    cls == obj // n
    cls == any // n
    cls == sub // n: related
    cls == fin // w: unrelated
    cls == eqs // w: unrelated
    cls == str // w: unrelated
    cls == lst // w: unrelated
    cls == Nil // w: unrelated
    cls == 1 // w: unrelated
    cls == intB // w: unrelated
    cls == new AnyRef() // n
  }

  locally {
    sub == cls // n: related
    sub == obj // n
    sub == fin // w: unrelated
  }

  locally {
    fin == cls // w: unrelated
    fin == 1 // w: non-sensible
    fin == str // w: non-sensible
    fin == lst // w: unrelated
    fin == Nil // w: non-sensible (both are final, unlike the line above)
    fin == new AnyRef() // w: final receiver, fresh
  }

  locally {
    eqs == obj // n
    eqs == cls // n: unrelated but custom equality (note that inverse warns "unrelated"). TODO scala/bug#6593
    eqs == 1   // n: but inverse warns "non-sensible"
  }

  locally {
    cc == obj // n
    cc == cls // w: non-sensible
    scc == cls // w: non-sensible (wrong)
    cc == 1 // w: non-sensible
  }

  locally {
    str == obj // n
    str == cls // n
    str == 1 // n
    str == "" // n
    str != opt // n
    str == null // n
  }

  locally {
    // Option has no `equals` override, unlike List. Some has synthetic case-equals
    opt == obj // n
    opt == cls // w: unrelated
    opt == 1 // w: unrelated
    opt == str // w: unrelated
    opt == lst // w: unrelated

    val som = Some(1)
    som == obj //n
    som == cls // w: non-sensible
    som == 1 // w: non-sensible
    som == str // w: non-sensible
  }

  locally {
    lst == obj // n
    lst == cls // n
    lst == 1 // n
    lst == opt // n
    lst == str // n
    lst == set // n
    lst == Seq(1) // n
    lst == map // n
    lst == eqs // n
  }

  locally {
    int == obj   // n
    int == cls   // w: non-sensible
    int == ""    // w: non-sensible
    int == 1L    // n
    int == true  // w: non-sensible
    int == intB  // n
    int == booB  // w: non-sensible
    int == ()    // w: non-sensible
    int == uniB // w: non-sensible
  }

  locally {
    null == obj  // n
    null == cls  // n
    null == int  // w: non-sensible
    null == intB // n
  }

  locally {
    intB == obj // n
    intB == cls // w: non-sensible
    intB == str // w: non-sensible
    intB == int // n
    intB == 1L  // n
    intB == null // n
    intB == true // w: non-sensible
    intB == booB // w: non-sensible
    intB == ()   // w: non-sensible
  }
}

/*
cooperative equality
  - used when `primitive == Any/AnyRef`
  - primitive is boxed, so it's a java.lang.Number
  - if both java.lang.Number
    - maintain scala semantics for boxed: (1: Any) equals (1L: Any) is true
    - if one is ScalaNumber, always use its `equals`. to support `1 == BigInt(1)`
  - also special cases java.lang.Character for ('a': Any) == 97
  - no special casing for strings / CharSequence
*/
