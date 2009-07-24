/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id:Relation.scala 6853 2006-03-20 16:58:47 +0100 (Mon, 20 Mar 2006) dubochet $


package scala.dbc
package statement;


/** A statement that returns a relation. */
abstract class Relation extends Statement {

  def isCompatibleType: (DataType,DataType)=>Boolean =
    ((dt,wdt)=>dt.isSubtypeOf(wdt));

  def typeCheck (relation: result.Relation): Unit = {
      val sameType: Boolean = (
        relation.metadata.length == fieldTypes.length &&
        (relation.metadata.zip(fieldTypes).forall({case Pair(field,expectedType) =>
          isCompatibleType(field.datatype, expectedType)}))
      );
      if (!sameType)
        throw new exception.IncompatibleSchema(fieldTypes,relation.metadata.map(field=>field.datatype));
  }

  def fieldTypes: List[DataType];

  def sqlTypeString: String =
    if (fieldTypes.isEmpty)
      "UNTYPED"
    else
      fieldTypes.map(dt=>dt.sqlString).mkString("RELATION (",", ",")");

  /** A SQL-99 compliant string representation of the statement. */
  def sqlString: String;

  /** A SQL-99 compliant string representation of the relation sub-
   * statement. This only has a meaning inside another statement. */
  def sqlInnerString: String;

  /** Executes the statement on the given database. */
  def execute (database: scala.dbc.Database): scala.dbc.result.Relation = {
    database.executeStatement(this);
  }

  def execute (database:scala.dbc.Database, debug:Boolean): scala.dbc.result.Relation = {
    database.executeStatement(this,debug);
  }

}
