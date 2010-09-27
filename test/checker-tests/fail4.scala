// Incompatible stacks in icode: BoxedUnit vs. empty.
class Classy {
  def f(b: Boolean): Unit = synchronized {
    if (b) ()
  }
}

// % work/check all -Xprint:icode work/fail4.scala
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
// [Now checking: constructors]
// [Now checking: flatten]
// [Now checking: mixin]
// [Now checking: cleanup]
// [[syntax trees at end of icode]]// Scala source: fail4.scala
// package <empty> {
//   class Classy extends java.lang.Object with ScalaObject {
//     def f(b: Boolean): Unit = {
//       Classy.this.synchronized(if (b)
//         scala.runtime.BoxedUnit.UNIT
//       else
//         scala.runtime.BoxedUnit.UNIT);
//       ()
//     };
//     def this(): Classy = {
//       Classy.super.this();
//       ()
//     }
//   }
// }
//
// [Now checking: icode]
//
// ** Checking class Classy
//
// ** Checking method Classy.f
// ** Checking Block 1 [S: 4, 3] [P: N/A]
//    1-> REFERENCE(class Classy)
// 0<- REFERENCE(class Classy)
//    1-> REFERENCE(class Classy)
//       2-> REFERENCE(class Classy)
//    1<- REFERENCE(class Classy)
// 0<- REFERENCE(class Classy)
// Output changed for Block 1 [S: 4, 3] [P: N/A]
// ** Checking Block 4 [S: 3, 6, 5] [P: 1]
//    1-> BOOL
// 0<- BOOL
// Output changed for Block 4 [S: 3, 6, 5] [P: 1]
// ** Checking Block 3 [S: N/A] [P: 1, 4, 5, 6, 7]
//    1-> REFERENCE(class Throwable)
//       2-> REFERENCE(class Object)
//    1<- REFERENCE(class Object)
// 0<- REFERENCE(class Throwable)
//    1-> REFERENCE(trait Nothing)
// ** Checking Block 6 [S: 3, 7] [P: 4]
//    1-> REFERENCE(class BoxedUnit)
// Output changed for Block 6 [S: 3, 7] [P: 4]
// ** Checking Block 5 [S: 3, 7] [P: 4]
//    1-> REFERENCE(class BoxedUnit)
// Output changed for Block 5 [S: 3, 7] [P: 4]
// Checker created new stack: (List(REFERENCE(class BoxedUnit)), List(REFERENCE(class BoxedUnit))) => List(REFERENCE(class BoxedUnit))
// TypeStack init: REFERENCE(class BoxedUnit)
// Checker created new stack: (List(REFERENCE(class BoxedUnit)), List(REFERENCE(class BoxedUnit))) => List(REFERENCE(class BoxedUnit))
// TypeStack init: REFERENCE(class BoxedUnit)
// Checker created new stack: (List(REFERENCE(class BoxedUnit)), List(REFERENCE(class BoxedUnit))) => List(REFERENCE(class BoxedUnit))
// TypeStack init: REFERENCE(class BoxedUnit)
// ** Checking Block 3 [S: N/A] [P: 1, 4, 5, 6, 7] with initial stack [REFERENCE(class BoxedUnit)]
// TypeStack init: REFERENCE(class BoxedUnit)
// 0<- REFERENCE(class BoxedUnit)
//    1-> REFERENCE(class Throwable)
//       2-> REFERENCE(class Object)
//    1<- REFERENCE(class Object)
// 0<- REFERENCE(class Throwable)
//    1-> REFERENCE(trait Nothing)
// ** Checking Block 7 [S: 3, 2] [P: 5, 6] with initial stack [REFERENCE(class BoxedUnit)]
// TypeStack init: REFERENCE(class BoxedUnit)
// 0<- REFERENCE(class BoxedUnit)
//    1-> REFERENCE(class Object)
// 0<- REFERENCE(class Object)
// Output changed for Block 7 [S: 3, 2] [P: 5, 6]
// Checker created new stack: (List(REFERENCE(class BoxedUnit)), List(REFERENCE(class BoxedUnit))) => List(REFERENCE(class BoxedUnit))
// TypeStack init: REFERENCE(class BoxedUnit)
// Exception in thread "main" scala.tools.nsc.backend.icode.CheckerException: Incompatible stacks: TypeStack(1 elems) {
//   REFERENCE(class BoxedUnit)
// } and TypeStack() in Classy.f at entry to block: 3
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
