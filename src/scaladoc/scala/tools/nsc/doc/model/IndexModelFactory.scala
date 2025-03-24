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

package scala
package tools.nsc
package doc
package model

import scala.collection.immutable.{SortedMap, SortedSet}
import scala.collection.mutable

object IndexModelFactory {

  def makeIndex(universe: Universe): Index = new Index {

    lazy val (firstLetterIndex, hasDeprecatedMembers): (Map[Char, SymbolMap], Boolean) = {

      object result {
        val map = mutable.HashMap.empty[Char, SymbolMap]
        var deprecated = false

        /* symbol name ordering */
        implicit def orderingMap: Ordering[String] = math.Ordering.String

        def addMember(d: MemberEntity): Unit = {
          val firstLetter = {
            val ch = d.name.head.toLower
            if(ch.isLetterOrDigit) ch else '_'
          }
          val map = this.map
          val letter = map.getOrElse(firstLetter, SortedMap.empty[String, SortedSet[MemberEntity]])
          val members = letter.getOrElse(d.name, SortedSet.empty[MemberEntity](Ordering.by { _.toString })) + d
          if (!deprecated && members.exists(_.deprecation.isDefined))
            deprecated = true
          map(firstLetter) = letter + (d.name -> members)
        }
      }

      //@scala.annotation.tailrec // TODO
      def gather(owner: DocTemplateEntity): Unit =
        for(m <- owner.members if m.inDefinitionTemplates.isEmpty || m.inDefinitionTemplates.head == owner)
          m match {
            case tpl: DocTemplateEntity =>
              result.addMember(tpl)
              gather(tpl)
            case non: MemberEntity if !non.isConstructor =>
              result.addMember(non)
            case x @ _ =>
          }

      gather(universe.rootPackage)

      (result.map.toMap, result.deprecated)
    }
  }
}
