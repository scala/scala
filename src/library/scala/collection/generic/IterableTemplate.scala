/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.generic
import scala.collection._

/** <p>
 *    A template trait for iterable collections.
 *  </p>
 *  <p>
 *    Collection classes mixing in this trait provide a method
 *    <code>iterator</code> which returns an iterator over all the
 *    elements contained in the collection. They also provide a method
 *    <code>newBuilder</code> which creates a builder for collections of the
 *    same kind.
 *  </p>
 *  <p>
 *    This trait implements <code>Traversable</code>'s <code>foreach</code>
 *    method by stepping through all elements. Subclasses of <code>Iterable</code>
 *    should re-implement <code>foreach</code> with something more efficient,
 *    if possible.
 *  </p>
 *  <p>
 *    This trait adds methods <code>iterator</code>, <code>sameElements</code>,
 *    <code>takeRight</code>, <code>dropRight</code> to the methods inherited
 *    from trait <a href="../Traversable.html" target="ContentFrame">
 *    <code>Traversable</code></a>.
 *  </p>
 *
 *  @author Martin Odersky
 *  @version 2.8
 */
trait IterableTemplate[+A, +This <: IterableTemplate[A, This] with Iterable[A]]
extends TraversableTemplate[A, This]
   with IterableLike[A, This]
