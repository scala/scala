/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.dbc
package statement


@deprecated(DbcIsDeprecated, "2.9.0") abstract class AccessMode {
  def sqlString: String
}

@deprecated(DbcIsDeprecated, "2.9.0") object AccessMode {
  case object ReadOnly extends AccessMode {
    def sqlString = "READ ONLY"
  }
  case object ReadWrite extends AccessMode {
    def sqlString = "READ WRITE"
  }
}
