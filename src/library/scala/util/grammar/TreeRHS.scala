/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.util.grammar

/** Right hand side of a tree production. */
abstract class TreeRHS

/** Right hand side of a tree production, labelled with a letter from an alphabet. */
case class LabelledRHS[A](label: A, hnt: Int) extends TreeRHS

case object AnyTreeRHS extends TreeRHS
