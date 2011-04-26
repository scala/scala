/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.dbc
package statement


/** A join behaviour in a <code>Jointure</code>. */
@deprecated(DbcIsDeprecated, "2.9.0") abstract class JoinType {
  /** A SQL-99 string representation of the join behaviour. */
  def sqlString: String
}

@deprecated(DbcIsDeprecated, "2.9.0") object JoinType {

  /** A join behaviour where a joined tuple is created only when a
   *  corresponding tuple exists in both original relations.
   */
  case object Inner extends JoinType {
    val sqlString = "INNER JOIN"
  }

  /** A join behaviour family where a joined tuple is created even when a
   *  tuple has no corresponding tuple in the other relation. The fields
   *  populated by values of the other tuple will receive the NULL value.
   */
  abstract class Outer extends JoinType

  object Outer {
    /** An outer join behaviour where there will be at least on tuple for
     *  every tuple in the left relation.
     */
    case object Left extends Outer {
      val sqlString = "LEFT OUTER JOIN"
    }
    /** An outer join behaviour where there will be at least on tuple for
     *  every tuple in the right relation.
     */
    case object Right extends Outer {
      val sqlString = "RIGHT OUTER JOIN"
    }
    /** An outer join behaviour where there will be at least on tuple for
     *  every tuple in both right and left relations.
     */
    case object Full extends Outer {
      val sqlString = "FULL OUTER JOIN"
    }
  }
}
