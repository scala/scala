/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id:Boolean.scala 6853 2006-03-20 16:58:47 +0100 (Mon, 20 Mar 2006) dubochet $


package scala.dbc
package datatype;


/** The SQL type for a truth value. */
class Boolean extends DataType {

  def isEquivalent (datatype:DataType) = datatype match {
    case dt:Boolean => true
    case _ => false
  }

  def isSubtypeOf (datatype:DataType) = isEquivalent(datatype);

  type NativeType = scala.Boolean;
  val nativeTypeId = DataType.BOOLEAN;

  /** A SQL-99 compliant string representation of the type. */
  override def sqlString: java.lang.String = "BOOLEAN";

}
