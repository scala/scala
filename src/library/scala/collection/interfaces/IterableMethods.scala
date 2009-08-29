/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.collection.interfaces

import scala.collection._
import generic._
import mutable.Buffer
import scala.reflect.ClassManifest

trait IterableMethods[+A, +This <: IterableTemplate[A, This] with Iterable[A]] extends TraversableMethods[A, This]
{
  def dropRight(n: Int): Iterable[A]
  def iterator: Iterator[A]
  def sameElements[B >: A](that: Iterable[B]): Boolean
  def takeRight(n: Int): Iterable[A]

  override def view: IterableView[A, This]
  override def view(from: Int, until: Int): IterableView[A, This]
}