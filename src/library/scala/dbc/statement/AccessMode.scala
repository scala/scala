/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2005, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.dbc.statement;

abstract class AccessMode {
	def sqlString: String;
}

object AccessMode {
	case object ReadOnly extends AccessMode {
		def sqlString = "READ ONLY";
	}
	case object ReadWrite extends AccessMode {
		def sqlString = "READ WRITE"
	}
}