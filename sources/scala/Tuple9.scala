// DO NOT EDIT. Automatically generated file!

/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala {
  case class Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9](_1: T1, _2: T2, _3: T3, _4: T4, _5: T5, _6: T6, _7: T7, _8: T8, _9: T9) extends scala.Object with {

    override def hashCode() = _1.hashCode() ^ _2.hashCode() ^ _3.hashCode() ^ _4.hashCode() ^ _5.hashCode() ^ _6.hashCode() ^ _7.hashCode() ^ _8.hashCode() ^ _9.hashCode();

    override def == (other: Any) =
      if (other is Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9]) {
	val that = other as Tuple9[T1, T2, T3, T4, T5, T6, T7, T8, T9];
	(_1 == that._1) && (_2 == that._2) && (_3 == that._3) && (_4 == that._4) && (_5 == that._5) && (_6 == that._6) && (_7 == that._7) && (_8 == that._8) && (_9 == that._9)
      } else Boolean.False;

    override def toString() = "(" + _1 + "," + _2 + "," + _3 + "," + _4 + "," + _5 + "," + _6 + "," + _7 + "," + _8 + "," + _9 + ")";
  }
}
