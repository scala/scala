// A test for the case where the outer field of class B#J should be eliminated.

import reflect.ClassTag

abstract class A {
  abstract class I

  val foo = this
}

class B extends A {
  class J extends I {
    val bar = foo
  }

  type II = I
  class K extends II {
    val bar = foo
  }

  class L extends (I @annotation.tailrec) {
    val bar = foo
  }
}


class C extends A {
  val c: C = this

  class M extends c.I {
    val bar = foo
  }
}


object Test extends App {
  val b = new B
  val c0 = new C
  val c = new { override val c = c0 } with C

  assert((new b.J).bar eq b)
  assert((new b.K).bar eq b)
  assert((new b.L).bar eq b)
  assert((new c.M).bar eq c)

  def checkOuterFields[C: ClassTag](expected: Int) {
    val cls = implicitly[ClassTag[C]].runtimeClass
    val outerFields = cls.getDeclaredFields().filter(_.getName.contains("$outer"))
    assert(outerFields.size == expected, outerFields.map(_.getName))
  }

  checkOuterFields[A#I](1) // the base class must have the $outer pointer
  checkOuterFields[B#J](0) // reuse parent class' $outer pointer
  checkOuterFields[B#K](0) // ... through an alias
  checkOuterFields[B#L](0) // ... through the annotated type
  checkOuterFields[C#M](1) // different prefix, can't share.
}
