/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.dbc.statement;

import scala.dbc.statement.expression._;

/** Data to be inserted into a table in an <code>Insert</code>. */
abstract class InsertionData {
	def sqlString: String;
}

object InsertionData {
	/** Insertion of data resulting from a query on the database. */
	case class Subquery (query:Relation) extends InsertionData {
		def sqlString = query.sqlString;
	}
	/** Insertion of data as explicitly defined values. */
	case class Constructor (
		columnNames:Option[List[String]],
		columnValues:List[Expression]
	) extends InsertionData {
		def sqlString = {
			(columnNames match {
				case None => ""
				case Some(cn) => cn.mkString(" (",", ",")")
			}) +
			" VALUES" +
			columnValues.map(e=>e.sqlInnerString).mkString(" (",", ",")")
		}
	}
}