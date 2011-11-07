// CC#9248 is conspicuously absent from the printed trees at every phase.
class A {
  def f[A, CC[X] <: Traversable[X]](): Unit = ()
}

// % work/check all -uniqid -Xprint:typer work/fail2.scala 
//
// TypeStack init: REFERENCE(type AnyRef#2783)
// [Not checkable: parser]
// [Not checkable: namer]
// [Not checkable: packageobjects]
// [[syntax trees at end of typer]]// Scala source: fail2.scala
// package <empty>#3 {
//   class A#9239 extends java.lang.Object#2488 with ScalaObject#1481 {
//     def this#9243(): A#9239 = {
//       A#9239.super.this#5850();
//       ()
//     };
//     def f#9244[A#9245 >: Nothing#5846 <: Any#46, CC#9246[X#11055 >: Nothing#5846 <: Any#46] >: [X#11055]Nothing#5846 <: [X#11055]Traversable#3199[X#11055]](): Unit#3819 = ()
//   }
// }
// 
// [Now checking: typer]
// [check: typer] work/fail2.scala:3: Expected owner type CC#9248, found type CC#9246: Trees$TypeDef / type X#11055>: Nothing#5846 <: Any#46
// [Now checking: superaccessors]
// [check: superaccessors] work/fail2.scala:3: Expected owner type CC#9248, found type CC#9246: Trees$TypeDef / type X#11055>: Nothing#5846 <: Any#46
// [Now checking: pickler]
// [check: pickler] work/fail2.scala:3: Expected owner type CC#9248, found type CC#9246: Trees$TypeDef / type X#11055>: Nothing#5846 <: Any#46
// [Now checking: refchecks]
// [check: refchecks] work/fail2.scala:3: Expected owner type CC#9248, found type CC#9246: Trees$TypeDef / type X#11055>: Nothing#5846 <: Any#46
// [Now checking: selectiveanf]
// [check: selectiveanf] work/fail2.scala:3: Expected owner type CC#9248, found type CC#9246: Trees$TypeDef / type X#11055>: Nothing#5846 <: Any#46
// [Now checking: liftcode]
// [check: liftcode] work/fail2.scala:3: Expected owner type CC#9248, found type CC#9246: Trees$TypeDef / type X#11055>: Nothing#5846 <: Any#46
// [Now checking: selectivecps]
// [check: selectivecps] work/fail2.scala:3: Expected owner type CC#9248, found type CC#9246: Trees$TypeDef / type X#11055>: Nothing#5846 <: Any#46
// [Now checking: uncurry]
// [check: uncurry] work/fail2.scala:3: Expected owner type CC#9248, found type CC#9246: Trees$TypeDef / type X#11055>: Nothing#5846 <: Any#46
// [Now checking: tailcalls]
// [check: tailcalls] work/fail2.scala:3: Expected owner type CC#9248, found type CC#9246: Trees$TypeDef / type X#11055>: Nothing#5846 <: Any#46
// [Not checkable: specialize]
// [Not checkable: explicitouter]
// [Now checking: erasure]
// [Now checking: lazyvals]
// [Now checking: lambdalift]
// [Now checking: constructors]
// [Now checking: flatten]
// [Now checking: mixin]
// [Now checking: cleanup]
// ...