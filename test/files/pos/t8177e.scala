trait T[A] { val foo: { type B = A } = ???; def bar(b: foo.B) = () }
object O extends T[Int] { bar(0) }

/*
val tp = T.<refinement>.type
tp.underlying = AnyRef{type B = Int}

val B = tyoeOf[t.foo.type forSome { val t: T[_] }].resultType.member(TermName("B"))
val A = typeOf[T[_]].typeParams.head
val refinementTypeInT = B.owner
A.asSeenFrom(tp, refinementTypeInT)

  tp.baseType(refinementTypeInT) // NoType

*/
