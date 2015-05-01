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

    lazy val (firstLetterIndex, hasDeprecatedMembers): (Map[Char, SymbolMap], Boolean) = {

      object result extends mutable.HashMap[Char,SymbolMap] {

        var deprecated = false

        /* symbol name ordering */
        implicit def orderingMap = math.Ordering.String

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
          if (!deprecated && members.find(_.deprecation.isDefined).isDefined)
            deprecated = true
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
            case non: MemberEntity if !non.isConstructor =>
              result.addMember(non)
            case x @ _ =>
          }

      gather(universe.rootPackage)

      (result.toMap, result.deprecated)
    }
  }
}
