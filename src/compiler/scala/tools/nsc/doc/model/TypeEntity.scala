/* NSC -- new Scala compiler
 * Copyright 2007-2010 LAMP/EPFL
 * @author  Manohar Jonnalagedda
 */

package scala.tools.nsc
package doc
package model

import scala.collection._

abstract class TypeEntity {

  /** A string representation of this type. */
  def name: String

  /** Maps which parts of this type's name reference other entities. The map is indexed by the position of the first
    * character that reference some entity, and contains the entity and the position of the last referenced
    * character. The referenced character ranges do not to overlap or nest. The map is sorted by position. */
  def refEntity: SortedMap[Int, (TemplateEntity, Int)]

  override def toString =
    name

}
