/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: Map.scala 16884 2009-01-09 16:52:09Z cunei $


package scala.collection.generic

/** The reason for this class is so that we can
 *  have both a generic immutable `+` with signature
 *
 *    def + [B1 >: B](kv: (A, B1)): Map[A, B1]
 *
 *  and a (deprecated) mutable `+` of signature
 *
 *    def + (kv: (A, B)): this.type = this += kv
 *
 *  The former is required to fulfill the Map contract.
 *  The latter is required for backwards compatibility.
 *  We can't have both methods in the same class, as that would give a double definition.
 *  They are OK in different classes though, and narrowly escape a `same erasure' problem.
 *  Once the deprecated + goes away we can do without class MutableMapTemplateBase.
 *
 *  @author Martin Odersky
 *  @version 2.8
 */
trait MutableMapTemplateBase[A, B, +This <: MutableMapTemplateBase[A, B, This] with mutable.Map[A, B]]
  extends MapTemplate[A, B, This] with Cloneable[This] {
  def + [B1 >: B] (kv: (A, B1)): mutable.Map[A, B1] =  clone().asInstanceOf[mutable.Map[A, B1]] += kv
}
