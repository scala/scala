import scala.Stream._

object consistencyError {
  /* this gives an error:
  Consistency problem compiling (virtual file)!
  Trying to call method body%1(List(scala.collection.immutable.Stream[A])) with arguments (List(tp2, temp6, temp5))
      case (l #:: ls, rs) => None
                              ^
  scala.tools.nsc.symtab.Types$TypeError: too many arguments for method body%1: (val rs: scala.collection.immutable.Stream[A])None.type

  two errors found
  vss(0) =
  args = List(tp2, temp6, temp5)
  vss(1) = value rs, value ls, value l
  args = List(tp2, temp6, temp5)
  targets(0) = FinalState(,scala.None)
  targets(1) = FinalState(,scala.None)
  labels(1) = method body%1
  labels(0) = method body%0
  bx = 1
  label.tpe = (val rs: scala.collection.immutable.Stream[A])None.type
  */
  def crash[A](lefts: Stream[A], rights: Stream[A]) = (lefts, rights) match {
    case (Stream.Empty, Stream.Empty) => None
    case (l #:: ls, rs) => None
  }

  // These work
  // def works1[A](lefts: Stream[A]) = lefts match {
  //   case Stream.Empty => None
  //   case l #:: ls => None
  // }
  //
  // def works2[A](lefts: Stream[A], rights: Stream[A]) = (lefts, rights) match {
  //   case (Stream.Empty, Stream.Empty) => None
  //   case (ls, rs) => None
  // }
}
