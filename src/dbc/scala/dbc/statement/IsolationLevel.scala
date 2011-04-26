/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.dbc
package statement


@deprecated(DbcIsDeprecated, "2.9.0") abstract class IsolationLevel {
  def sqlString: String
}

@deprecated(DbcIsDeprecated, "2.9.0") object IsolationLevel {
  case object ReadUncommitted extends IsolationLevel {
    def sqlString = "ISOLATION LEVEL READ UNCOMMITTED"
  }
  case object ReadCommitted extends IsolationLevel {
    def sqlString = "ISOLATION LEVEL READ COMMITTED"
  }
  case object RepeatableRead extends IsolationLevel {
    def sqlString = "ISOLATION LEVEL REPEATABLE READ"
  }
  case object Serializable extends IsolationLevel {
    def sqlString = "ISOLATION LEVEL SERIALIZABLE"
  }
}
