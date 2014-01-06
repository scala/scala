/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author  Manohar Jonnalagedda
 */

package scala.tools.nsc
package doc
package model

import scala.collection._

/** A type. Note that types and templates contain the same information only for the simplest types. For example, a type
  * defines how a template's type parameters are instantiated (as in `List[Cow]`), what the template's prefix is
  * (as in `johnsFarm.Cow`), and supports compound or structural types. */
abstract class TypeEntity {

  /** The human-readable representation of this type. */
  def name: String

  /** Maps which parts of this type's name reference entities. The map is indexed by the position of the first
    * character that reference some entity, and contains the entity and the position of the last referenced
    * character. The referenced character ranges do not to overlap or nest. The map is sorted by position. */
  def refEntity: SortedMap[Int, (base.LinkTo, Int)]

  /** The human-readable representation of this type. */
  override def toString = name
}
