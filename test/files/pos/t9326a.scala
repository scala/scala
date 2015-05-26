package p

trait M[A]

class C extends M[Tuple1[X] forSome { type X }]

