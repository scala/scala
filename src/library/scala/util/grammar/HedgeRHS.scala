/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.util.grammar

@deprecated("This class will be removed", "2.10.0")
abstract class HedgeRHS

/** Right hand side of a hedge production, deriving a single tree. */
@deprecated("This class will be removed", "2.10.0")
case class ConsRHS(tnt: Int, hnt: Int) extends HedgeRHS

/** Right hand side of a hedge production, deriving any hedge. */
@deprecated("This class will be removed", "2.10.0")
case object AnyHedgeRHS extends HedgeRHS

/** Right hand side of a hedge production, deriving the empty hedge. */
@deprecated("This class will be removed", "2.10.0")
case object EmptyHedgeRHS extends HedgeRHS
