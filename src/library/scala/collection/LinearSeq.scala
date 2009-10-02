/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.collection

import generic._
import mutable.Builder

/** <p>
 *    Class <code>Linear[A]</code> represents linear sequences of elements.
 *    For such sequences <code>isEmpty</code>, <code>head</code> and
 *    <code>tail</code> are guaranteed to be efficient constant time (or near so)
 *    operations.<br/>
 *    It does not add any methods to <code>Seq</code> but overrides several
 *    methods with optimized implementations.
 *  </p>
 *
 *  @author  Martin Odersky
 *  @author  Matthias Zenger
 *  @version 1.0, 16/07/2003
 *  @since   2.8
 */
trait LinearSeq[+A] extends Seq[A]
                            with GenericTraversableTemplate[A, LinearSeq]
                            with LinearSeqLike[A, LinearSeq[A]] {
  override def companion: GenericCompanion[LinearSeq] = LinearSeq
}

/**
 *  @since 2.8
 */
object LinearSeq extends SeqFactory[LinearSeq] {
  implicit def builderFactory[A]: BuilderFactory[A, LinearSeq[A], Coll] = new VirtualBuilderFactory[A]
  def newBuilder[A]: Builder[A, LinearSeq[A]] = immutable.LinearSeq.newBuilder[A]
}
