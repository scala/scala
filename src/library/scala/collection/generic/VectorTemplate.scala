/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2006-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.collection
package generic

/** Sequences that support O(1) element access and O(1) length computation.
 *  This class does not add any methods to Sequence but overrides several
 *  methods with optimized implementations.
 *
 *  @author Sean McDirmid
 *  @author Martin Odersky
 *  @version 2.8
 */
trait VectorTemplate[+A, +This <: VectorTemplate[A, This] with Vector[A]] extends SequenceTemplate[A, This] with VectorLike[A, This]
