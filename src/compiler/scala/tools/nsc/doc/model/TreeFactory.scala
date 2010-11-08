package scala.tools.nsc
package doc
package model

/** The goal of this trait is , using makeTree,
  * to browse a tree to
  * 1- have the String of the complete tree (tree.expression)
  * 2- fill references to create hyperLinks later in html.pageTemplate
  *
  * It is applied in ModelFactory => makeTree
  *
  */

trait TreeFactory {
  thisTreeFactory: ModelFactory with TreeFactory =>
  val global: Global

  import global._

  def makeTree(rhs: Tree): TreeEntity = {

    val printSteps: Boolean = false
    val tree = new TreeEntity

    try {

      val firstIndex = rhs.pos.startOrPoint

      /** Gets the full string of the right hand side of a parameter, without links */
      def makeExpression(rhs: Tree){
        val start = rhs.pos.startOrPoint
        val end = rhs.pos.endOrPoint
        var expr = ""
        for (i <- start until end) expr += rhs.pos.source.content.apply(i)
        rhs match {
          case Block(r,s) => expr += "}"
          case _ =>
        }
        tree.expression += expr
      }

      val traverser = new Traverser {

        /** Finds the Entity on which we will later create a link on,
          * stores it in tree.refs with its position
          */
        def makeLink(rhs: Tree){
          var start = rhs.pos.point - firstIndex
          val end = rhs.pos.endOrPoint - firstIndex
          if(start != end) {
            var asym = rhs.symbol
            if (asym.isClass) makeTemplate(asym) match{
              case docTmpl: DocTemplateImpl =>
                tree.refs += ((start,(docTmpl,end)))
              case _ =>
            }
            else if (asym.isTerm && asym.owner.isClass){
              if (asym.isSetter) asym = asym.getter(asym.owner)
              makeTemplate(asym.owner) match {
                case docTmpl: DocTemplateImpl =>
                  val mbrs: List[MemberImpl] = makeMember(asym,docTmpl)
                  mbrs foreach {mbr =>
                    tree.refs += ((start,(mbr,end)))
                  }
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

      makeExpression(rhs)
      traverser.traverse(rhs)
    }
    catch {
      case e: Throwable =>
        //println("Bad tree: " + rhs)
    }

    tree

  }

}
