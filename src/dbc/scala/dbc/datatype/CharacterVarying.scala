/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.dbc
package datatype;


/** A SQL type for a varying length string of characters with arbitrary
 *  maximal length and arbitrary character set.
 */
abstract class CharacterVarying extends CharacterString {

  def isEquivalent(datatype: DataType) = datatype match {
    case dt: CharacterVarying =>
      length == dt.length && encoding == dt.encoding
    case _ =>
      false
  }

  def isSubtypeOf(datatype: DataType) = datatype match {
    case dt: CharacterVarying =>
      length >= dt.length && encoding == dt.encoding
    case _ =>
      false
  }

  /** The maximal length of the string defined in characters. */
  def length: Int;

  /** A SQL-99 compliant string representation of the type. */
  override def sqlString: java.lang.String =
    "CHARACTER VARYING (" + length.toString() + ")";

}
