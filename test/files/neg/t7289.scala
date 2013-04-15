object Test extends App {
  trait Schtroumpf[T]

  implicit def schtroumpf[T, U <: Coll[T], Coll[X] <: Traversable[X]]
    (implicit minorSchtroumpf: Schtroumpf[T]): Schtroumpf[U] = ???

  implicit val qoo: Schtroumpf[Int] = new Schtroumpf[Int]{}
  implicitly[Schtroumpf[Nil.type]]
}

/*
info1 = {scala.tools.nsc.typechecker.Implicits$ImplicitInfo@3468}"qoo: => Test.Schtroumpf[Int]"
info2 = {scala.tools.nsc.typechecker.Implicits$ImplicitInfo@3469}"schtroumpf: [T, U <: Coll[T], Coll[_] <: Traversable[_]](implicit minorSchtroumpf: Test.Schtroumpf[T])Test.Schtroumpf[U]"
isStrictlyMoreSpecific(info1, info2)
  isSubType(Test.Schtroumpf[Int], Test.Schtroumpf[U] forSome { T; U <: Coll[T]; Coll[_] <: Traversable[_] })
    isAsSpecificValueType(Test.Schtroumpf[Int], Test.Schtroumpf[U], undef2 = List(type T, type U, type Coll))

      val et: ExistentialType = Test.Schtroumpf[U] forSome { T; U <: Coll[T]; Coll[_] <: Traversable[_] }
      val tp1 = Test.Schtroumpf[Int]
      et.withTypeVars(isSubType(tp1, _, depth))
        solve()
        tvars = tList(=?Nothing, =?Int, =?=?Int)


[    create] ?T                       ( In Test#schtroumpf[T,U <: Coll[T],Coll[_] <: Traversable[_]] )
[    create] ?U                       ( In Test#schtroumpf[T,U <: Coll[T],Coll[_] <: Traversable[_]] )
[    create] ?Coll                    ( In Test#schtroumpf[T,U <: Coll[T],Coll[_] <: Traversable[_]] )
[   setInst] Nothing                  ( In Test#schtroumpf[T,U <: Coll[T],Coll[_] <: Traversable[_]], T=Nothing )
[   setInst] scala.collection.immutable.Nil.type( In Test#schtroumpf[T,U <: Coll[T],Coll[_] <: Traversable[_]], U=scala.collection.immutable.Nil.type )
[   setInst] =?scala.collection.immutable.Nil.type( In Test#schtroumpf[T,U <: Coll[T],Coll[_] <: Traversable[_]], Coll==?scala.collection.immutable.Nil.type )
[    create] ?T                       ( In Test#schtroumpf[T,U <: Coll[T],Coll[_] <: Traversable[_]] )
[   setInst] Int                      ( In Test#schtroumpf[T,U <: Coll[T],Coll[_] <: Traversable[_]], T=Int )
[    create] ?T                       ( In Test#schtroumpf[T,U <: Coll[T],Coll[_] <: Traversable[_]] )
[    create] ?U                       ( In Test#schtroumpf[T,U <: Coll[T],Coll[_] <: Traversable[_]] )
[    create] ?Coll                    ( In Test#schtroumpf[T,U <: Coll[T],Coll[_] <: Traversable[_]] )
[   setInst] Nothing                  ( In Test#schtroumpf[T,U <: Coll[T],Coll[_] <: Traversable[_]], T=Nothing )
[   setInst] Int                      ( In Test#schtroumpf[T,U <: Coll[T],Coll[_] <: Traversable[_]], U=Int )
[   setInst] =?Int                    ( In Test#schtroumpf[T,U <: Coll[T],Coll[_] <: Traversable[_]], Coll==?Int )
*/