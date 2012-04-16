package scala.reflect.reify
package phases

import scala.runtime.ScalaRunTime.isAnyVal
import scala.runtime.ScalaRunTime.isTuple
import scala.reflect.reify.codegen._

trait Reify extends Symbols
               with Types
               with Names
               with Trees
               with AnnotationInfos
               with Positions
               with Util {

  self: Reifier =>

  import mirror._
  import definitions._
  import treeInfo._

  /**
   *  Reifies any supported value.
   *  For internal use only, use ``reified'' instead.
   */
  var currents: List[Any] = reifee :: Nil
  def current = currents.head
  def reify(reifee: Any): Tree = {
    currents = reifee :: currents
    try {
      reifee match {
        // before adding some case here, in global scope, please, consider
        // whether it can be localized like reifyAnnotationInfo or reifyScope
        // this will help reification stay as sane as possible
        case sym: Symbol              => reifySymRef(sym)
        case tpe: Type                => reifyType(tpe)
        case name: Name               => reifyName(name)
        case tree: Tree               => reifyTree(tree)
        // disabled because this is a very special case that I plan to remove later
        // why do I dislike annotations? see comments to `reifyAnnotationInfo`
//        case ann: AnnotationInfo      => reifyAnnotationInfo(ann)
        case pos: Position            => reifyPosition(pos)
        case mods: mirror.Modifiers   => reifyModifiers(mods)
        case xs: List[_]              => reifyList(xs)
        case s: String                => Literal(Constant(s))
        case v if isAnyVal(v)         => Literal(Constant(v))
        case null                     => Literal(Constant(null))
        case _                        =>
          throw new Error("reifee %s of type %s is not supported".format(reifee, reifee.getClass))
      }
    } finally {
      currents = currents.tail
    }
  }
}