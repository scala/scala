/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.dbc.statement;

abstract class IsolationLevel {
	def sqlString: String;
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