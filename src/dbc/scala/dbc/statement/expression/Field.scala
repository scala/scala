/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.dbc
package statement
package expression;


abstract class Field extends Expression {

  /** The name of the schema in the database where the field is located. */
  def schemaName: Option[String] = None;

  /** The name of the table in the database where the field is located. */
  def tableName: Option[String];

  /** The name of the field in the database. */
  def fieldName: String;

  /** A SQL-99 compliant string representation of the relation sub-
   * statement. This only has a meaning inside another statement. */
  def sqlInnerString: String = (
    (schemaName match {
      case None => ""
      case Some(sn) => sn + "."
    }) +
    (tableName match {
      case None => ""
      case Some(tn) => tn + "."
    }) + fieldName
  )

}
