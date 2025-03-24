/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package doc
package model

import scala.collection.immutable.SortedMap

/** A fragment of code. */
abstract class TreeEntity {

  /** The human-readable representation of this abstract syntax tree. */
  def expression: String

  /** Maps which parts of this syntax tree's name reference entities. The map is indexed by the position of the first
    * character that reference some entity, and contains the entity and the position of the last referenced
    * character. The referenced character ranges do not to overlap or nest. The map is sorted by position. */
  def refEntity: SortedMap[Int, (Entity, Int)]

  /** The human-readable representation of this abstract syntax tree. */
  override def toString = expression

}
