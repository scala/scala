/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.doc

import scala.collection._

trait Index {
  type SymbolMap = SortedMap[String, SortedSet[model.MemberEntity]]

  def firstLetterIndex: Map[Char, SymbolMap]

  def hasDeprecatedMembers: Boolean
}
