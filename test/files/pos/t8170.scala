object O {
  trait X
  trait B extends A {
    override type T[F1 <: X] = F1
  }
  trait A {
    type T[F <: X]
  }
}

object Test {
  import O._
  val a: B = ???
  val b: a.T[X] = ???
  b.ensuring(x => true) // trigger an implicit search
}


/*
this = {AliasArgsTypeRef@3004}"Test#7680.a#14899.T#14823[O#7702.X#7793]"
  sym = type T#14823
    info = namer: [F#14824 <: O#7703.X#7793]F#14824
result = {AbstractNoArgsTypeRef@3237}"F#24451"
tp = {PolyType@3235}"[F#14824 <: O#7703.X#7793]F#14824"
tparams = 
  (0)  = {AbstractTypeSymbol@3247}"type F#24451"
*/