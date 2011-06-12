/* NSC -- new Scala compiler
 * Copyright 2007-2011 LAMP/EPFL
 * @author  Pedro Furlanetto
 */

package scala.tools.nsc
package doc
package model

import scala.collection._

object IndexModelFactory {

  def makeIndex(universe: Universe): Index = new Index {

    lazy val firstLetterIndex: Map[Char, SymbolMap] = {

      val result = new mutable.HashMap[Char,SymbolMap] {

        /* Owner template ordering */
        implicit def orderingSet = math.Ordering.String.on { x: MemberEntity => x.name.toLowerCase }
        /* symbol name ordering */
        implicit def orderingMap = math.Ordering.String.on { x: String => x.toLowerCase }

        def addMember(d: MemberEntity) = {
          val firstLetter = {
            val ch = d.name.head.toLower
            if(ch.isLetterOrDigit) ch else '_'
          }
          val letter = this.get(firstLetter).getOrElse {
            immutable.SortedMap[String, SortedSet[MemberEntity]]()
          }
          val members = letter.get(d.name).getOrElse {
            SortedSet.empty[MemberEntity](Ordering.by { _.toString })
          } + d
          this(firstLetter) = letter + (d.name -> members)
        }

      }

      //@scala.annotation.tailrec // TODO
      def gather(owner: DocTemplateEntity): Unit =
        for(m <- owner.members if m.inDefinitionTemplates.isEmpty || m.inDefinitionTemplates.head == owner)
          m match {
            case tpl: DocTemplateEntity =>
              result.addMember(tpl)
              gather(tpl)
            case alias: AliasType =>
              result.addMember(alias)
            case absType: AbstractType =>
              result.addMember(absType)
            case non: NonTemplateMemberEntity if !non.isConstructor =>
              result.addMember(non)
            case x @ _ =>
          }

      gather(universe.rootPackage)

      result.toMap

    }

  }

}
