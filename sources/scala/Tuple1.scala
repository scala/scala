// DO NOT EDIT. Automatically generated file!

/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala {
  case class Tuple1[T1](_1: T1) extends scala.Object with {

    override def hashCode() = _1.hashCode();

    override def == (other: Any) =
      if (other is Tuple1[T1]) {
	val that = other as Tuple1[T1];
	(_1 == that._1)
      } else Boolean.False;

    override def toString() = "[" + _1  + "]";
  }
}
