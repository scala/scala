// DO NOT EDIT. Automatically generated file!

/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala {
  case class Tuple4[T1, T2, T3, T4](_1: T1, _2: T2, _3: T3, _4: T4) extends scala.Object with {

    override def hashCode() = _1.hashCode() ^ _2.hashCode() ^ _3.hashCode() ^ _4.hashCode();

    override def == (other: Any) =
      if (other is Tuple4[T1, T2, T3, T4]) {
	val that = other as Tuple4[T1, T2, T3, T4];
	(_1 == that._1) && (_2 == that._2) && (_3 == that._3) && (_4 == that._4)
      } else Boolean.False;

    override def toString() = "(" + _1 + "," + _2 + "," + _3 + "," + _4 + ")";
  }
}
