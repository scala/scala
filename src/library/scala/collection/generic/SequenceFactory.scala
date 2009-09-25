/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection
package generic

/** A template for companion objects of Sequence and subclasses thereof.
 */
abstract class SequenceFactory[CC[X] <: Sequence[X] with GenericTraversableTemplate[X, CC]] extends TraversableFactory[CC] {

  /** This method is called in a pattern match { case Sequence(...) => }.
   *
   *  @param x the selector value
   *  @return  sequence wrapped in an option, if this is a Sequence, otherwise none
   */
  def unapplySeq[A](x: CC[A]): Some[CC[A]] = Some(x)
}
