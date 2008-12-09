/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Iterable.scala 15188 2008-05-24 15:01:02Z stepancheg $


package scalax.collection

import generic._
import immutable.Nil

/** An ordered collection is a collection with a fixed sequence of elements
 *  which corresponds to append order. In particular, it holds that
 *
 *    (c1 ++ c2).elements = c1.elements ++ c2.elements
 *
 *  for any two ordered collections c1 and c2.
 *  Ordered collections support
 *    - operations that form subsequences: take, takeWhile, drop, dropWhile,
 *    - zip, unzip
 *
 *  @author Martin Odersky
 *  @version 2.8
 */
trait OrderedIterable[+A] extends Iterable[A] with covariant.OrderedIterableTemplate[OrderedIterable, A]

/** Various utilities for instances of <a href="Iterable.html">Iterable</a>.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 */
object OrderedIterable extends covariant.IterableFactory[OrderedIterable] {

  val empty: OrderedIterable[Nothing] = Nil

}
