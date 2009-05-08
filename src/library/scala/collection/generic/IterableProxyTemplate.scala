/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: IterableProxy.scala 15458 2008-06-28 20:23:22Z stepancheg $


package scala.collection.generic

import collection.mutable.Buffer

// Methods could be printed by  cat IterableTemplate.scala | egrep '^  (override )?def'


/** This trait implements a proxy for iterable objects. It forwards
 *  all calls to a different iterable object
 *
 *  @author  Martin Odersky
 *  @version 2.8
 */
trait IterableProxyTemplate[+A, +This <: IterableTemplate[A, This] with Iterable[A]] extends IterableTemplate[A, This] with TraversableProxyTemplate[A, This] {
  override def elements: Iterator[A] = self.elements
  override def takeRight(n: Int): This = self.takeRight(n)
  override def dropRight(n: Int): This = self.dropRight(n)
  override def sameElements[B >: A](that: Iterable[B]): Boolean = self.sameElements(that)
  override def view = self.view
  override def view(from: Int, until: Int) = self.view(from, until)
}
