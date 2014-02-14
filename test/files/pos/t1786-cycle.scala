trait GenTraversableLike[+A, +Repr] extends Any

object O {
  (null: Any) match {
    case _: LongTraversableLike[_] =>
  }
}

trait LongTraversable extends LongTraversableLike[LongTraversable]

trait LongTraversableLike[+Repr <: LongTraversableLike[Repr]] extends GenTraversableLike[Any, Repr]

/*
% scalac-hash v2.11.0-M8 test/files/pos/t1786-cycle.scala
[warn] v2.11.0-M8 failed, using closest available
test/files/pos/t1786-cycle.scala:11: error: illegal cyclic reference involving trait LongTraversableLike
trait LongTraversableLike[+Repr <: LongTraversableLike[Repr]] extends GenTraversableLike[Any, Repr]
                                                                      ^
one error found

Okay again after SI-1786 was reverted.


|-- object O BYVALmode-EXPRmode (site: package <empty>)
|    |-- super EXPRmode-POLYmode-QUALmode (silent: <init> in O)
|    |    |-- this EXPRmode (silent: <init> in O)
|    |    |    \-> O.type
|    |    \-> O.type
|    |-- (null: Any) match { case (_: LongTraversableLike[(_ @ <em... BYVALmode-EXPRmode (site: value <local O> in O)
|    |    |-- (null: Any) BYVALmode-EXPRmode (site: value <local O> in O)
|    |    |    |-- Any TYPEmode (site: value <local O> in O)
|    |    |    |    \-> Any
|    |    |    |-- null : pt=Any EXPRmode (site: value <local O> in O)
|    |    |    |    \-> Null(null)
|    |    |    \-> Any
|    |    |-- (_: LongTraversableLike[(_ @ <empty>)]) : pt=Any PATTERNmode (site: value <local O> in O) enrichment only
|    |    |    |-- LongTraversableLike[(_ @ <empty>)] TYPEPATmode-TYPEmode (site: value <local O> in O) enrichment only
|    |    |    |    |--  <: LongTraversableLike[Repr] TYPEmode (site: type Repr in <empty>)
|    |    |    |    |    |-- LongTraversableLike[Repr] TYPEmode (site: type Repr in <empty>)
|    |    |    |    |    |    |-- Repr NOmode (site: type Repr in <empty>)
|    |    |    |    |    |    |    \-> Repr
|    |    |    |    |    |    \-> LongTraversableLike[Repr]
|    |    |    |    |    [adapt]  <: LongTraversableLike[Repr] is now a TypeTree( <: LongTraversableLike[Repr])
|    |    |    |    |    \->  <: LongTraversableLike[Repr]
|    |    |    |    |-- (_ @ <empty>) TYPEPATmode-TYPEmode (site: value <local O> in O) enrichment only
|    |    |    |    |    \-> _
|    |    |    |    |-- GenTraversableLike FUNmode-TYPEmode (site: trait LongTraversableLike)
|    |    |    |    |    \-> GenTraversableLike
|    |    |    |    |-- GenTraversableLike[Any, Repr] TYPEmode (site: trait LongTraversableLike)
|    |    |    |    |    |-- Any TYPEmode (site: trait LongTraversableLike)
|    |    |    |    |    |    \-> Any
|    |    |    |    |    |-- Repr TYPEmode (site: trait LongTraversableLike)
|    |    |    |    |    |    \-> Repr
|    |    |    |    |    caught scala.reflect.internal.Symbols$CyclicReference: illegal cyclic reference involving trait LongTraversableLike: while typing GenTraversableLike[Any, Repr]
test/files/pos/t1786-cycle.scala:11: error: illegal cyclic reference involving trait LongTraversableLike
trait LongTraversableLike[+Repr <: LongTraversableLike[Repr]] extends GenT
*/