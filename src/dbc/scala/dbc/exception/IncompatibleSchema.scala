/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.dbc
package exception


/** A type category for all SQL types that store constant-precision numbers. */
@deprecated(DbcIsDeprecated) case class IncompatibleSchema (
  expectedSchema: List[DataType],
  foundSchema: List[DataType]
) extends Exception;
