package tastytest

object RecTypes {

  // ******** MEMBER REFERENCES ********
  // A refinement member can only reference a symbol within the refinements parent or outer scope
  // i.e. the owner of the referenced symbol is one of the parents
  // the type reference however will be a path dependent type from the refinement class symbol
  // This can be a reference to a structural refinement if the parent is a refinement type
  // Illegal (`type T = d.type` refers to `d` which is not defined on `AnyRef`):
  //    class DDepStructural[Ref <: ({ type T = d.type }) { val d: Int }]
  // OK (the first refinement defines a structural d, which the outer refinement can reference):
  //    class DDepStructural[Ref <: ({ val d: Int }) { type T = d.type }]
  // ***********************************

  trait C {
    type T
    type U
    val t: T
    val u: U
  }

  class CDep[C2 <: C { type U = T }] { // 1 level RECthis, using a TYPEREF
    def test_==(c: C2): Boolean = c.t == c.u
  }

  class CDepNest[C2 <: (C { type U = T }) { type T = Int }] { // 2 level nested RECthis, using a TYPEREF, normalised before pickling
    def test_==(c: C2): Boolean = c.t == c.u
  }

  class CDepNestProject[U2 <: ((C { type U = T }) { type T = Int }) # U] { // 2 level nested RECthis, using a TYPEREF, normalised before pickling
    def widenU(u: U2): Int = u
  }

  class CDepDeep[C2 <: C { type T <: C { type T = Int; type U = T}; type U = T }] {
    def test_==(c: C2): Boolean =
      c.t.t == c.t.u && c.t.t == c.u.t && c.t.t == c.u.u && c.t.u == c.u.t && c.t.u == c.u.u && c.u.t == c.u.u
  }

  trait D {
    val d: Any
    type T
    val t: T
  }

  class DDep[D2 <: D { type T = d.type }] { // 1 level RECthis, using a TERMREF
    def test_==(d: D2): Boolean = d.d == d.t
  }

  class DDepNest[D2 <: (D { type T = d.type }) { val d: Int }] { // 2 level nested RECthis, using a TERMREF, normalised before pickling
    def test_==(d: D2): Boolean = d.d == d.t
  }

  class DDepNestProject[T2 <: ((D { type T = d.type }) { val d: Int }) # T] { // 2 level nested RECthis, using a TERMREF, normalised before pickling
    def widenT(t: T2): Int = t
  }

  trait E {
    val d: Any
    val e: Any
    type T
    type U
  }

  class EDep[E2 <: E { type T = d.type; type U = e.type }] { // 1 level RECthis, twice, using a TERMREF
    def test_==(e: E2): Boolean = e.d == e.e
  }

  class StructuralTypeAlias[Ref <: { type T = Int; val t: T }] {
    import reflect.Selectable.reflectiveSelectable
    def get(ref: Ref): ref.T = ref.t
  }

  class StructuralTypeBounds[Ref <: { type T <: Int; val t: T }] {
    import reflect.Selectable.reflectiveSelectable
    def get(ref: Ref): ref.T = ref.t
  }

}
