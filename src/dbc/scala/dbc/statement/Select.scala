/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.dbc
package statement


/** A statement that when executed on a database will return a relation.
 * The returned relation will be a subset of a table in the database or
 * a jointure between such subsets. */
@deprecated(DbcIsDeprecated, "2.9.0") abstract class Select extends Relation {

  /** Defines if duplicated tuples should be removed from the returned
   * relation. <h3>Compatibility notice</h3> Some DBMS (PostgreSQL) allow
   * uniqueness constrains on an arbitrary field instead of the entire
   * tuple. */
  def setQuantifier: Option[SetQuantifier]

  /** Defines the output fields that a tuple in the returned relation will
   * contain, and their content with respect to the tables in the
   * database. If the fields are not specified (that is the list is
   * empty), all possible input fields will be returned. <h3>Compatibility
   * notice</h3> SQL's qualified asterisk select sublist is not
   * available. */
  def selectList: List[DerivedColumn]

  /** Defines the relations from which the query will obtain its data.*/
  def fromClause: List[Relation]

  /** Defines condition that must be true in the returned relation's tuples.
   *  This value expression must return a boolean or boolean-compatible
   *  value. This condition is applied before any GROUP BY clause.
   */
  def whereClause: Option[Expression]

  /** Defines the grouping of the returned relation's tuples. One tuple is
   *  returned for every group. The value of <code>selectList</code> must
   *  use aggregate functions for calculation.
   */
  def groupByClause: Option[List[Expression]]

  /** Defines conditions that must be true in the returned relation's tuples.
   *  The value expression must return a boolean can only refer to fields
   *  that are grouped or to any field from inside an aggregate function.
   */
  def havingClause: Option[Expression]

  /* def windowClause: Option[_]; */

  /** A SQL-99 compliant string representation of the select statement. */
  def sqlString: String = (
    "SELECT" +
    (setQuantifier match {
      case None => ""
      case Some(sq) => " " + sq.sqlString
    }) +
    (selectList match {
      case Nil => " *"
      case _ => (" " + selectList.tail.foldLeft(selectList.head.sqlString)
                 ((name:String, dc:DerivedColumn) => name + ", " + dc.sqlString))
    }) +
    (fromClause match {
      case Nil => sys.error("Empty from clause is not allowed")
      case _ => (" FROM " + fromClause.tail.foldLeft(fromClause.head.sqlInnerString)
      ((name:String, rel:Relation) => name + ", " + rel.sqlInnerString))
    }) +
    (whereClause match {
      case None => ""
      case Some(expr) => " WHERE " + expr.sqlInnerString
    }) +
    (groupByClause match {
      case None => ""
      case Some(gbl) => gbl match {
        case Nil => sys.error("Empty group by clause is not allowed")
        case _ =>
          (" GROUP BY " +
           gbl.tail.foldLeft(gbl.head.sqlInnerString)
           ((name:String, gb) => name + ", " + gb.sqlInnerString))
      }
    }) +
    (havingClause match {
      case None => ""
      case Some(expr) => " HAVING " + expr.sqlString
    })
  );

  /** A SQL-99 compliant string representation of the relation sub-
   *  statement. This only has a meaning inside a query.
   */
  def sqlInnerString: String = "("+sqlString+")"

}
