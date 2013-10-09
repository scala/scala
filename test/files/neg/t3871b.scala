/**

The protected modifier applies to class member definitions. Protected members of a class can be accessed from within

  0a. the companion module of any of those classes

A protected identifier x may be used as a member name in a selection r.x only
if one of the following applies:
  1a. The access is within the template defining the member, or,
  if a qualification C is given,
  1b. inside the package C, or
  1c. the class C , or its companion module, or
  2.  r is one of the reserved words this and super, or
  3.  r’s type conforms to a type-instance of the class which contains the access.

  4. A different form of qualification is protected[this]. A member M marked with this
     modifier is called object-protected; it can be accessed only from within the object
     in which it is defined. That is, a selection p.M is only legal if the prefix is this
     or O.this, for some class O enclosing the reference. In addition, the restrictions
     for unqualified protected apply.
*/

object E {
  val e = new E
  import e._
  def n(a: A, b: B, c: C) = {
    b.protE    // 1c
    c.protE    // 1c
    a.protE    // 1c
    A.protOE   // 1c
  }
}

class E {
  object A {
    protected def protO = 2
    protected[E] def protOE = 3
    protected[this] def protOT = 3
  }
  class A {
    protected def prot = 2
    protected[E] def protE = 3
    protected[this] def protT = 4

    // 1a
    prot; protE; protT
    def foo = {prot; protE; protT}
    new { prot; protE }
    def this(a: Any) = {this(); prot; protE; protT}
    object B extends A {
      A.this.prot
      A.this.protE
      A.this.protT
    }

    import A._
    // 0a
    protO
    // 3
    protOE
    protOT // not allowed
  }

  class B extends A {
    // 1b
    this.prot; this.protE;
    super.prot; super.protE;

    // 4
    this.protT
    // 4 !!! "or the super keyword"
    super.protT

    def n(a: A, b: B, c: C) = {
      b.prot    // 3
      c.prot    // 3
      a.prot    // not allowed, prefix type `A` does not conform to `B`

      b.protT   // not allowed
      c.protT   // not allowed
      a.protT   // not allowed
    }
  }
  object B {
    def n(a: A, b: B, c: C) = {
      b.prot    // 3 !!!
      c.prot    // 3 !!!
      // Wording of 3 seems insufficient, missing:
      // "... (if the access is from a class), or
      // the type instance of companion class (if the access is from a module)"
      a.prot    // not allowed

      b.protT   // not allowed
      c.protT   // not allowed
      a.protT   // not allowed
    }
  }
  class C extends B

  class Z {
    def n(a: A, b: B, c: C) = {
      b.prot    // not allowed
      c.prot    // not allowed
      a.prot    // not allowed
      b.protE   // 2
      a.protE   // 2
      c.protE   // 2

      b.protT   // not allowed
      c.protT   // not allowed
      a.protT   // not allowed
    }
  }
}

class Other {
  val e = new E
  import e._
  def n(a: A, b: B, c: C) = {
    b.prot    // not allowed
    c.prot    // not allowed
    a.prot    // not allowed
    b.protE   // not allowed
    a.protE   // not allowed
    c.protE   // not allowed
  }
}
