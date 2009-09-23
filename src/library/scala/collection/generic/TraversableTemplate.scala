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
 *    A template trait for traversable collections.
 *    This is a base trait of all kinds of Scala collections. It implements
 *    the behavior common to all collections, in terms of a method
 *    <code>foreach</code> with signature:
 *  </p><pre>
 *   <b>def</b> foreach[U](f: Elem => U): Unit</pre>
 *  <p>
 *    Collection classes mixing in this trait provide a concrete
 *    <code>foreach</code> method which traverses all the
 *    elements contained in the collection, applying a given function to each.
 *    They also need to provide a method <code>newBuilder</code>
 *    which creates a builder for collections of the same kind.
 *  </p>
 *  <p>
 *    A traversable class might or might not have two properties: strictness
 *    and orderedness. Neither is represented as a type.
 *  </p>
 *  <p>
 *    The instances of a strict collection class have all their elements
 *    computed before they can be used as values. By contrast, instances of
 *    a non-strict collection class may defer computation of some of their
 *    elements until after the instance is available as a value.
 *    A typical example of a non-strict collection class is a
 *    <a href="../immutable/Stream.html" target="ContentFrame">
 *    <code>scala.collection.immutable.Stream</code></a>.
 *    A more general class of examples are <code>TraversableViews</code>.
 *  </p>
 *  <p>
 *    If a collection is an instance of an ordered collection class, traversing
 *    its elements with <code>foreach</code> will always visit elements in the
 *    same order, even for different runs of the program. If the class is not
 *    ordered, <code>foreach</code> can visit elements in different orders for
 *    different runs (but it will keep the same order in the same run).<br/>
 *    A typical example of a collection class which is not ordered is a
 *    <code>HashMap</code> of objects. The traversal order for hash maps will
 *    depend on the hash codes of its elements, and these hash codes might
 *    differ from one run to the next. By contrast, a <code>LinkedHashMap</code>
 *    is odered because it's <code>foreach</code> method visits elements in the
 *    order they were inserted into the <code>HashMap</code>.
 *  </p>
 *
 *  @author Martin Odersky
 *  @version 2.8
 */
trait TraversableTemplate[+A, +This <: TraversableTemplate[A, This] with Traversable[A]]
extends TraversableLike[A, This]

