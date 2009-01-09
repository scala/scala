/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Iterable.scala 15188 2008-05-24 15:01:02Z stepancheg $


package scalax.collection.generic.covariant

import annotation.unchecked.uncheckedVariance

/** Collection classes mixing in this class provide a method
 *  <code>elements</code> which returns an iterator over all the
 *  elements contained in the collection.
 *
 *  @note If a collection has a known <code>size</code>, it should also sub-type <code>Collection</code>.
 *        Only potentially unbounded collections should directly sub-class <code>Iterable</code>.
 *  @author  Matthias Zenger
 *  @version 1.1, 04/02/2004
 */
trait IterableTemplate[+CC[+B] <: IterableTemplate[CC, B] with Iterable[B], +A]
   extends generic.IterableTemplate[CC, A @uncheckedVariance] { self /*: CC[A]*/ => }

// !!! todo: explain why @uncheckedVariance is justified here.

