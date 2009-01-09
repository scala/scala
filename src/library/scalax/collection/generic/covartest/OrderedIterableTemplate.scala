/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Iterable.scala 15188 2008-05-24 15:01:02Z stepancheg $


package scalax.collection.generic.covartest

import OrderedIterable._

/** Ordered iterables are iterables where the `elements` method always returns elements in the same
 *  order (namely the order in which elements were appended to the iterable). In particular, one has
 *  for every two ordered iterables `xs` and `ys`:
 *
 *  `(xs ++ ys).elements = xs.elements ++ ys.elements
 */
trait OrderedIterableTemplate[+CC[+B] <: OrderedIterableTemplate[CC, B] with OrderedIterable[B], +A]
  extends IterableTemplate[CC, A]
