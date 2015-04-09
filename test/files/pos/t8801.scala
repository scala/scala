sealed trait Nat {
  type Prev <: Nat { type Succ = Nat.this.type }
  type Succ <: Nat { type Prev = Nat.this.type }
}
 
object Nat {
  object Zero extends Nat {
    type Prev = Nothing
  }
 
  type _0 = Zero.type
  type _1 = _0#Succ
  type _2 = _1#Succ
  type _3 = _2#Succ
  type _4 = _3#Succ
  type _5 = _4#Succ
  type _6 = _5#Succ
  type _7 = _6#Succ
  type _8 = _7#Succ
  type _9 = _8#Succ
}
