/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.dbc
package datatype;


/** A SQL type for an unbounded length string of characters with arbitrary
  * character set. */
class CharacterLargeObject extends CharacterString {

  def isEquivalent (datatype:DataType) = datatype match {
    case dt:CharacterLargeObject => {
      encoding == dt.encoding
    }
    case _ => false
  }

  def isSubtypeOf (datatype:DataType) = isEquivalent(datatype);

  /** A SQL-99 compliant string representation of the type. */
  override def sqlString: java.lang.String = "CHARACTER LARGE OBJECT";

}
