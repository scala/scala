/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id:IsolationLevel.scala 6853 2006-03-20 16:58:47 +0100 (Mon, 20 Mar 2006) dubochet $


package scala.dbc
package statement


abstract class IsolationLevel {
  def sqlString: String
}

object IsolationLevel {
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
