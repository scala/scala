/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection.jcl;

/** Creates a buffer backed by a Java array list.
 *
 *  @author Sean McDirmid
 */
class ArrayList[A](override val underlying : java.util.ArrayList) extends RandomAccessSeq.Mutable[A] with BufferWrapper[A]  {
  def this() = this(new java.util.ArrayList);
  override def elements = super[BufferWrapper].elements;

  trait Projection0[A] extends MutableSeq.Projection[A] with RandomAccessSeq.Projection[A] {
    override def projection : Projection0[A] = this
    override def elements : SeqIterator[Int,A] = new DefaultSeqIterator

    protected class MapProjection[B](f : A => B) extends super.MapProjection[B](f) with Projection0[B] {
      override def projection = this
    }
    override def map[B](f: A => B) : Projection0[B] = new MapProjection[B](f)
  }
  class Projection extends Buffer.Projection[A] with RandomAccessSeq.MutableProjection[A] with Projection0[A] {
    override def elements : BufferIterator[Int,A] = new DefaultBufferIterator
    override def projection : Projection = this
  }
  override def projection : Projection = new Projection
}
