object ProjTest {
  trait MInt { type Type }
  trait _0 extends MInt { type Type = Boolean }
  trait Succ[Pre <: MInt] extends MInt { type Type = Pre#Type }

  type _1 = Succ[_0]
  type _2 = Succ[_1]

  type X1 = _0#Type   // Ok
  type X2 = _1#Type   // Ok
  type X3 = _2#Type   // Compiler error, illegal cyclic reference involving type Type
}

