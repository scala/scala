// DO NOT EDIT. Automatically generated file!

/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala {
  case class Tuple2[T1, T2](_1: T1, _2: T2) extends scala.Object with {

    override def hashCode() = _1.hashCode() ^ _2.hashCode();

    override def == (other: Any) =
      if (other is Tuple2[T1, T2]) {
	val that = other as Tuple2[T1, T2];
	(_1 == that._1) && (_2 == that._2)
      } else Boolean.False;

    override def toString() = "(" + _1 + "," + _2 + ")";
  }
}
