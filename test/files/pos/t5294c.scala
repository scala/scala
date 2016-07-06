sealed trait Nat {
  type IsZero[U, A <: U, B <: U] <: U
}

final object Zero extends Nat {
  type IsZero[U, T <: U, F <: U] = T
}

final class Succ[N <: Nat] extends Nat {
  type IsZero[U, T <: U, F <: U] = F
}

trait HList {
  type Head
  type Tail <: HList
  type Drop[N <: Nat] = N#IsZero[HList, this.type, Tail]
  type Apply[N <: Nat] = Drop[N]#Head // typechecks as HList.this.Head
}

object Test {
  type ::[H, T <: HList] = HList { type Head = H; type Tail = T}
  type HNil <: HList
  type T = Int :: String :: HNil

  type U = T#Drop[Succ[Zero.type]]#Head
  type V = T#Apply[Succ[Zero.type]]
  var u: U = ???
  var v: V = ???
  u = v
}
