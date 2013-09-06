/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author  Pedro Furlanetto
 */

package scala
package tools.nsc
package doc
package model

import scala.collection._

object IndexModelFactory {

  def makeIndex(universe: Universe): Index = new Index {

    lazy val firstLetterIndex: Map[Char, SymbolMap] = {

      object result extends mutable.HashMap[Char,SymbolMap] {

        def firstLetter(name: String): Char = {
          val ch = name.head.toLower
          if(ch.isLetterOrDigit) ch else '_'
        }

        def letter(firstLetter: Char): SortedMap[String, SortedSet[MemberEntity]] = this.get(firstLetter).getOrElse {
          immutable.SortedMap[String, SortedSet[MemberEntity]]()
        }

        /* symbol name ordering */
        implicit def orderingMap = math.Ordering.String

        def addMember(d: MemberEntity) = {
          val dFirstLetter = firstLetter(d.name)
          val dLetter = letter(dFirstLetter)
          val members = dLetter.get(d.name).getOrElse {
            SortedSet.empty[MemberEntity](Ordering.by { _.toString })
          } + d
          d.useCaseOf.flatMap(_.comment).flatMap(_.canonical).foreach { canonicalName =>
            val firstCanonicalLetter = firstLetter(canonicalName)
            this(firstCanonicalLetter) = letter(firstCanonicalLetter) + (canonicalName -> members)
          }
          this(dFirstLetter) = dLetter + (d.name -> members)
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

      result.toMap

    }

  }

}
