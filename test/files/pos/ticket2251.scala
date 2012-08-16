
// Martin: I am not sure this is a solvable problem right now. I'll leave it in pending.
// derived from pos/t1001
class A
trait B[T <: B[T]] extends A
class C extends B[C]
class D extends B[D]

class Data {
  // force computing lub of C and D (printLubs enabled:)

/*
lub of List(D, C) at depth 2
  lub of List(D, C) at depth 1
    lub of List(D, C) at depth 0
    lub of List(D, C) is A
  lub of List(D, C) is B[_1] forSome { type _1 >: D with C <: A }
lub of List(D, C) is B[_2] forSome { type _2 >: D with C{} <: B[_1] forSome { type _1 >: D with C{} <: A } }
*/
// --> result = WRONG

  // should be: B[X] forSome {type X <: B[X]} -- can this be done automatically? for now, just detect f-bounded polymorphism and fall back to more coarse approximation

  val data: List[A] = List(new C, new D)

  val data2 = List(new C, new D)

  val data3: List[B[X] forSome { type X <: B[_ <: A] }] = List(new C, new D)

  // Not yet --
  // val data4: List[B[X] forSome { type X <: B[X] }] = List(new C, new D)
  // <console>:7: error: type mismatch;
  //  found   : List[B[_ >: D with C <: B[_ >: D with C <: A]]]
  //  required: List[B[X] forSome { type X <: B[X] }]
  //        val data4: List[B[X] forSome { type X <: B[X] }] = List(new C, new D)

  // works
  val data5 = List[B[X] forSome { type X <: B[X] }](new C, new D)
}
