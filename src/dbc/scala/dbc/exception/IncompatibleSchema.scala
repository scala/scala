/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id:IncompatibleSchema.scala 6853 2006-03-20 16:58:47 +0100 (Mon, 20 Mar 2006) dubochet $


package scala.dbc
package exception


/** A type category for all SQL types that store constant-precision numbers. */
case class IncompatibleSchema (
  expectedSchema: List[DataType],
  foundSchema: List[DataType]
) extends Exception;
