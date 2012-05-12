/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.util.grammar

/** Right hand side of a tree production. */
@deprecated("This class will be removed", "2.10.0")
abstract class TreeRHS

/** Right hand side of a tree production, labelled with a letter from an alphabet. */
@deprecated("This class will be removed", "2.10.0")
case class LabelledRHS[A](label: A, hnt: Int) extends TreeRHS

@deprecated("This class will be removed", "2.10.0")
case object AnyTreeRHS extends TreeRHS
