package scala.tools.nsc
package transform

import collection.mutable.Buffer

abstract class Statics extends Transform with ast.TreeDSL {
  import global._

  class StaticsTransformer extends Transformer {

    /** finds the static ctor DefDef tree within the template if it exists. */
    def findStaticCtor(template: Template): Option[Tree] =
      template.body find {
        case defdef @ DefDef(_, nme.CONSTRUCTOR, _, _, _, _) => defdef.symbol.hasStaticFlag
        case _ => false
      }

    /** changes the template for the class so that it contains a static constructor with symbol fields inits,
      * augments an existing static ctor if one already existed.
      */
    def addStaticInits(template: Template, newStaticInits: Buffer[Tree], localTyper: analyzer.Typer): Template = {
      if (newStaticInits.isEmpty)
        template
      else {
        val newCtor = findStaticCtor(template) match {
          // in case there already were static ctors - augment existing ones
          // currently, however, static ctors aren't being generated anywhere else
          case Some(ctor @ DefDef(_,_,_,_,_,_)) =>
            // modify existing static ctor
            deriveDefDef(ctor) {
              case block @ Block(stats, expr) =>
                // need to add inits to existing block
                treeCopy.Block(block, newStaticInits.toList ::: stats, expr)
              case term: TermTree =>
                // need to create a new block with inits and the old term
                treeCopy.Block(term, newStaticInits.toList, term)
            }
          case _ =>
            // create new static ctor
            val staticCtorSym  = currentClass.newStaticConstructor(template.pos)
            val rhs            = Block(newStaticInits.toList, Literal(Constant(())))

            localTyper.typedPos(template.pos)(DefDef(staticCtorSym, rhs))
        }
        deriveTemplate(template)(newCtor :: _)
      }
    }
  }
}
