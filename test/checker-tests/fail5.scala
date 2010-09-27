// Incompatible stacks in icode: BooleanRef vs. empty.
// This may be the same issue as fail4.scala, with the synchronized
// block being introduced in the rewriting.
class Crashy {
  def go(x1: Int) = {
    lazy val x2 = x1 < 0

    x2
  }
}

// % work/check all -Xprint:icode work/fail5.scala
// TypeStack init: REFERENCE(type AnyRef)
// [Not checkable: parser]
// [Not checkable: namer]
// [Not checkable: packageobjects]
// [Now checking: typer]
// [Now checking: superaccessors]
// [Now checking: pickler]
// [Now checking: refchecks]
// [Now checking: selectiveanf]
// [Now checking: liftcode]
// [Now checking: selectivecps]
// [Now checking: uncurry]
// [Now checking: tailcalls]
// [Not checkable: specialize]
// [Not checkable: explicitouter]
// [Now checking: erasure]
// [Now checking: lazyvals]
// [Now checking: lambdalift]
// [check: lambdalift] NoPosition: Apply arguments to new scala.runtime.BooleanRef contains an empty tree: List(<empty>)
// [Now checking: constructors]
// [check: constructors] NoPosition: Apply arguments to new scala.runtime.BooleanRef contains an empty tree: List(<empty>)
// [Now checking: flatten]
// [check: flatten] NoPosition: Apply arguments to new scala.runtime.BooleanRef contains an empty tree: List(<empty>)
// [Now checking: mixin]
// [check: mixin] NoPosition: Apply arguments to new scala.runtime.BooleanRef contains an empty tree: List(<empty>)
// [Now checking: cleanup]
// [[syntax trees at end of icode]]// Scala source: fail5.scala
// package <empty> {
//   class Crashy extends java.lang.Object with ScalaObject {
//     def go(x1$1: Int): Boolean = {
//       @volatile var bitmap$0$1: scala.runtime.VolatileIntRef = new scala.runtime.VolatileIntRef(0);
//       lazy var x2$lzy$1: scala.runtime.BooleanRef = new scala.runtime.BooleanRef(<empty>);
//       Crashy.this.x2$1(x1$1, x2$lzy$1, bitmap$0$1)
//     };
//     final <stable> private[this] def x2$1(x1$1: Int, x2$lzy$1: scala.runtime.BooleanRef, bitmap$0$1: scala.runtime.VolatileIntRef): Boolean = {
//       if (bitmap$0$1.elem.&(1).==(0))
//         {
//           Crashy.this.synchronized({
//             if (bitmap$0$1.elem.&(1).==(0))
//               {
//                 x2$lzy$1.elem = x1$1.<(0);
//                 bitmap$0$1.elem = bitmap$0$1.elem.|(1);
//                 ()
//               };
//             scala.runtime.BoxedUnit.UNIT
//           });
//           ()
//         };
//       x2$lzy$1.elem
//     };
//     def this(): Crashy = {
//       Crashy.super.this();
//       ()
//     }
//   }
// }
//
// [Now checking: icode]
//
// ** Checking class Crashy
//
// ** Checking method Crashy.go
// ** Checking Block 1 [S: N/A] [P: N/A]
//    1-> REFERENCE(class VolatileIntRef)
// 0<- REFERENCE(class VolatileIntRef)
//    1-> REFERENCE(class VolatileIntRef)
//       2-> REFERENCE(class VolatileIntRef)
//          3-> INT
//       2<- INT
//    1<- REFERENCE(class VolatileIntRef)
// 0<- REFERENCE(class VolatileIntRef)
//    1-> REFERENCE(class BooleanRef)
// 0<- REFERENCE(class BooleanRef)
//    1-> REFERENCE(class BooleanRef)
//       2-> REFERENCE(class BooleanRef)
//          3-> BOOL
//       2<- BOOL
//    1<- REFERENCE(class BooleanRef)
// 0<- REFERENCE(class BooleanRef)
//    1-> REFERENCE(class Crashy)
//       2-> INT
//          3-> REFERENCE(class BooleanRef)
//             4-> REFERENCE(class VolatileIntRef)
//          3<- REFERENCE(class VolatileIntRef)
//       2<- REFERENCE(class BooleanRef)
//    1<- INT
// 0<- REFERENCE(class Crashy)
//    1-> BOOL
// 0<- BOOL
//
// ** Checking method Crashy.x2$1
// ** Checking Block 1 [S: 4, 2] [P: N/A]
//    1-> REFERENCE(class VolatileIntRef)
// 0<- REFERENCE(class VolatileIntRef)
//    1-> INT
//       2-> INT
//    1<- INT
// 0<- INT
//    1-> INT
//       2-> INT
//    1<- INT
// 0<- INT
// Output changed for Block 1 [S: 4, 2] [P: N/A]
// ** Checking Block 4 [S: N/A] [P: 1, 10]
//    1-> REFERENCE(class BooleanRef)
// 0<- REFERENCE(class BooleanRef)
//    1-> BOOL
// 0<- BOOL
// ** Checking Block 2 [S: 7, 6] [P: 1]
//    1-> REFERENCE(class Crashy)
// 0<- REFERENCE(class Crashy)
//    1-> REFERENCE(class Crashy)
//       2-> REFERENCE(class Crashy)
//    1<- REFERENCE(class Crashy)
// 0<- REFERENCE(class Crashy)
// Output changed for Block 2 [S: 7, 6] [P: 1]
// ** Checking Block 7 [S: 6, 10, 8] [P: 2]
//    1-> REFERENCE(class VolatileIntRef)
// 0<- REFERENCE(class VolatileIntRef)
//    1-> INT
//       2-> INT
//    1<- INT
// 0<- INT
//    1-> INT
//       2-> INT
//    1<- INT
// 0<- INT
// Output changed for Block 7 [S: 6, 10, 8] [P: 2]
// ** Checking Block 6 [S: N/A] [P: 2, 7, 8, 10, 11, 12, 13]
//    1-> REFERENCE(class Throwable)
//       2-> REFERENCE(class Object)
//    1<- REFERENCE(class Object)
// 0<- REFERENCE(class Throwable)
//    1-> REFERENCE(trait Nothing)
// ** Checking Block 10 [S: 6, 4] [P: 7, 13]
//    1-> REFERENCE(class BoxedUnit)
// 0<- REFERENCE(class BoxedUnit)
//    1-> REFERENCE(class Object)
// 0<- REFERENCE(class Object)
// Output changed for Block 10 [S: 6, 4] [P: 7, 13]
// ** Checking Block 8 [S: 6, 12, 11] [P: 7]
//    1-> REFERENCE(class BooleanRef)
//       2-> INT
//          3-> INT
//       2<- INT
//    1<- INT
// Output changed for Block 8 [S: 6, 12, 11] [P: 7]
// Exception in thread "main" scala.tools.nsc.backend.icode.CheckerException: Incompatible stacks: TypeStack(1 elems) {
//   REFERENCE(class BooleanRef)
// } and TypeStack() in Crashy.x2$1 at entry to block: 6
//  at scala.tools.nsc.backend.icode.Checkers$ICodeChecker.meet2$1(Checkers.scala:165)
//  at scala.tools.nsc.backend.icode.Checkers$ICodeChecker$$anonfun$meet$2.apply(Checkers.scala:174)
//  at scala.tools.nsc.backend.icode.Checkers$ICodeChecker$$anonfun$meet$2.apply(Checkers.scala:174)
//  at scala.collection.LinearSeqOptimized$class.foldLeft(LinearSeqOptimized.scala:123)
//  at scala.collection.immutable.List.foldLeft(List.scala:45)
//  at scala.collection.LinearSeqOptimized$class.reduceLeft(LinearSeqOptimized.scala:137)
//  at scala.collection.immutable.List.reduceLeft(List.scala:45)
//  at scala.tools.nsc.backend.icode.Checkers$ICodeChecker.meet(Checkers.scala:174)
//  at scala.tools.nsc.backend.icode.Checkers$ICodeChecker$$anonfun$check$5.apply(Checkers.scala:140)
//  at scala.tools.nsc.backend.icode.Checkers$ICodeChecker$$anonfun$check$5.apply(Checkers.scala:140)
//  at scala.collection.LinearSeqOptimized$class.foreach(LinearSeqOptimized.scala:61)
//  at scala.collection.immutable.List.foreach(List.scala:45)
//  at scala.tools.nsc.backend.icode.Checkers$ICodeChecker.check(Checkers.scala:140)
//  at scala.tools.nsc.backend.icode.Checkers$ICodeChecker.check(Checkers.scala:110)
//  at scala.tools.nsc.backend.icode.Checkers$ICodeChecker$$anonfun$check$3.apply(Checkers.scala:103)
//  at scala.tools.nsc.backend.icode.Checkers$ICodeChecker$$anonfun$check$3.apply(Checkers.scala:103)
//  at scala.collection.LinearSeqOptimized$class.foreach(LinearSeqOptimized.scala:61)
//  at scala.collection.immutable.List.foreach(List.scala:45)
//  at scala.tools.nsc.backend.icode.Checkers$ICodeChecker.check(Checkers.scala:103)
//  at scala.tools.nsc.backend.icode.Checkers$ICodeChecker$$anonfun$checkICodes$1.apply(Checkers.scala:81)
//  at scala.tools.nsc.backend.icode.Checkers$ICodeChecker$$anonfun$checkICodes$1.apply(Checkers.scala:81)
//  at scala.collection.mutable.HashMap$$anon$2$$anonfun$foreach$3.apply(HashMap.scala:89)
//  at scala.collection.mutable.HashMap$$anon$2$$anonfun$foreach$3.apply(HashMap.scala:89)
//  at scala.collection.Iterator$class.foreach(Iterator.scala:631)
//  at scala.collection.mutable.HashTable$$anon$1.foreach(HashTable.scala:161)
//  at scala.collection.mutable.HashTable$class.foreachEntry(HashTable.scala:194)
//  at scala.collection.mutable.HashMap.foreachEntry(HashMap.scala:39)
//  at scala.collection.mutable.HashMap$$anon$2.foreach(HashMap.scala:89)
//  at scala.tools.nsc.backend.icode.Checkers$ICodeChecker.checkICodes(Checkers.scala:81)
//  at scala.tools.nsc.Global$Run.compileSources(Global.scala:759)
//  at scala.tools.nsc.Global$Run.compile(Global.scala:823)
//  at scala.tools.nsc.Main$.process(Main.scala:106)
//  at scala.tools.nsc.Main$.main(Main.scala:120)
//  at scala.tools.nsc.Main.main(Main.scala)
// [paulp@leaf trunk (check-all)]$
