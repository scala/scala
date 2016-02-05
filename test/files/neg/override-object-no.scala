// See also pos/override-object-yes.scala

package case1 {
  // Missing interface in overriding object
  class Bippy { def f = 1 }
  trait Bippo

  trait Foo {
    object Bar extends Bippy with Bippo { override def f = 2 }
    def f(x: Bippo)
    def g = f(Bar)
  }
  trait Foo2 extends Foo {
    override object Bar extends Bippy {  // err
      override def f = 3
    }
  }

  // type mismatch in member
  trait Quux1 { object Bar { def g = 55 } }
  trait Quux2 extends Quux1 { override object Bar { def g = "abc" } } // err

  // still can't override final objects!
  trait Quux3 { final object Bar { } }
  trait Quux4 extends Quux3 { override object Bar  } // err
}

// type parameter as-seen-from business
package case2 {
  // invariance (see pos for the covariant case)
  class Bar[T]

  class Foo[T] {
    object A extends Bar[T]
  }

  class Baz[S] extends Foo[S] {
    override object A extends Bar[S]
  }

  class P1 extends Foo[Traversable[String]]
  class P2 extends P1 {
    override object A extends Bar[List[String]]  // err
  }
}

// Both overridden and overriding members must be objects, not vals with a module type
object SI9574 {
  object Foo
  object Bar
  trait A { def x: Foo.type }
  trait B extends A { def x: Bar.type } // should not compile (SI-9574)
  trait C extends A { override object x }
  trait D { object x; def y = x }
  trait E extends D { override val x: super.x.type = y } // OK but doesn't need object subtyping exception
}
