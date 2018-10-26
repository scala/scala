// asSeenFrom crash related to BaseTypeSeq bug for singleton types
trait Poly[T] { type TT = T
  def foo: (this.type with Any)#TT
}

// equivalent:
// class C { def meh[T](x: Poly[T]): (x.type with Any)#TT = ??? }
