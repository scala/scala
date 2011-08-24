abstract class F {
    type mType <: M
}

abstract class M { self =>

    type mType <: M

    type fType = F {type mType >: self.mType }
    def fs: List[fType]
}

abstract class A0 extends M {
    type mType = A0
    def fs: List[fType] = Nil
}

object A extends A0 {}

abstract class B0 extends M {
    type mType = B0
    def fs: List[fType] = Nil
}

object B extends B0 {}

object C {
    def ab = List(A) ::: List(B)
    // the following compiles successfully:
    // def ab = List(A) ::: List[M](B)
}