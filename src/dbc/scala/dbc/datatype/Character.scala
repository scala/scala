/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.dbc
package datatype;


/** A SQL type for a string of characters of arbitrary length with
 *  arbitrary character set.
 */
abstract class Character extends CharacterString {

  def isEquivalent(datatype: DataType) = datatype match {
    case dt: Character =>
      length == dt.length && encoding == dt.encoding
    case _ =>
      false
  }

  def isSubtypeOf(datatype: DataType) = datatype match {
    case dt: Character =>
      length >= dt.length && encoding == dt.encoding
    case _ =>
      false
  }

  /** The length of the string defined in characters. */
  def length: Int;

  /** A SQL-99 compliant string representation of the type. */
  override def sqlString: java.lang.String = "CHARACTER (" + length.toString() + ")";

}
