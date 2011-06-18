/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.util.grammar

abstract class HedgeRHS

/** Right hand side of a hedge production, deriving a single tree. */
case class ConsRHS(tnt: Int, hnt: Int) extends HedgeRHS

/** Right hand side of a hedge production, deriving any hedge. */
case object AnyHedgeRHS extends HedgeRHS

/** Right hand side of a hedge production, deriving the empty hedge. */
case object EmptyHedgeRHS extends HedgeRHS
