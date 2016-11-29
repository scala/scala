class Test {
  def test = {

    val l1 = null: Int #: String #: Boolean #: String #: HNil.type

    type _2 = Succ[Succ[Zero.type]]

    val t1: Boolean = null.asInstanceOf[ l1.type#Drop[_2]#Head ]

    val t2: Boolean = null.asInstanceOf[ l1.type#Apply[_2] ]
  }
}


sealed trait Nat {
  type Fold[U, F[_ <: U] <: U, Z <: U] <: U
}

final object Zero extends Nat {
  type Fold[U, F[_ <: U] <: U, Z <: U] = Z
}

final class Succ[N <: Nat] extends Nat {
  type Fold[U, F[_ <: U] <: U, Z <: U] = F[N#Fold[U, F, Z]]
}

trait HList {
  type Head
  type Tail <: HList
  type Drop[N <: Nat] = N#Fold[HList, ({ type L[X <: HList] = X#Tail })#L, this.type]
  type Apply[N <: Nat] = Drop[N]#Head
}

class #: [H, T <: HList] extends HList { type Head = H; type Tail = T }

object HNil extends HList { type Head = Nothing;  type Tail = Nothing }
