package scala.tools.nsc
package doc
package model

import scala.collection._
import scala.reflect.internal.util.{RangePosition, OffsetPosition, SourceFile}

/** The goal of this trait is , using makeTree,
  * to browse a tree to
  * 1- have the String of the complete tree (tree.expression)
  * 2- fill references to create hyperLinks later in html.pageTemplate
  *
  * It is applied in ModelFactory => makeTree
  *
  */

trait TreeFactory { thisTreeFactory: ModelFactory with TreeFactory =>

  val global: Global
  import global._

  def makeTree(rhs: Tree): Option[TreeEntity] = {

    var expr = new StringBuilder
    var refs = new immutable.TreeMap[Int, (Entity, Int)] // start, (Entity to be linked to , end)

    rhs.pos match {
      case pos: RangePosition => {
        val source: SourceFile = pos.source
        val firstIndex = pos.startOrPoint
        val lastIndex = pos.endOrPoint

        assert(firstIndex < lastIndex, "Invalid position indices for tree " + rhs + " (" + firstIndex + ", " + lastIndex + ")")
        expr.appendAll(source.content, firstIndex, lastIndex - firstIndex)

        val traverser = new Traverser {

          /** Finds the Entity on which we will later create a link on,
           * stores it in tree.refs with its position
           */
          def makeLink(rhs: Tree){
            var start = pos.startOrPoint - firstIndex
            val end = pos.endOrPoint - firstIndex
            if(start != end) {
              var asym = rhs.symbol
              if (asym.isClass) makeTemplate(asym) match{
                case docTmpl: DocTemplateImpl =>
                  refs += ((start, (docTmpl,end)))
                case _ =>
              }
              else if (asym.isTerm && asym.owner.isClass){
                if (asym.isSetter) asym = asym.getter(asym.owner)
                makeTemplate(asym.owner) match {
                  case docTmpl: DocTemplateImpl =>
                    val mbrs: Option[MemberImpl] = findMember(asym, docTmpl)
                    mbrs foreach { mbr => refs += ((start, (mbr,end))) }
                  case _ =>
                }
              }
            }
          }
          /**
           * Goes through the tree and makes links when a Select occurs,
           * The case of New(_) is ignored because the object we want to create a link on
           * will be reached with recursivity and we don't want a link on the "new" string
           * If a link is not created, its case is probably not defined in here
           */
          override def traverse(tree: Tree) = tree match {
            case Select(qualifier, name) =>
              qualifier match {
                case New(_) =>
                  case _ => makeLink(tree)
              }
            traverse(qualifier)
            case Ident(_) => makeLink(tree)
            case _ =>
              super.traverse(tree)
          }
        }

        traverser.traverse(rhs)

        Some(new TreeEntity {
          val expression = expr.toString
          val refEntity = refs
        })
      }
      case pos: OffsetPosition =>
        Some(new TreeEntity {
          val expression = rhs.toString
          val refEntity = new immutable.TreeMap[Int, (Entity, Int)]
        })
      case _ => None
    }
  }
}
