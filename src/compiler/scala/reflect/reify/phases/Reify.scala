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

  // `reify` looked so nice, I wanted to push the last bit of orthogonal
  // logic out of it so you can see the improvement.  There is no cost to
  // wrapper methods of this form because the inliner will eliminate them,
  // but they are very good at separating concerns like pushing/popping
  // a stack, and they are great for composition and reuse.
  //
  // Also, please avoid public vars whenever possible.
  private object reifyStack {
    var currents: List[Any] = reifee :: Nil

    @inline final def push[T](reifee: Any)(body: => T): T = {
      currents ::= reifee
      try body
      finally currents = currents.tail
    }
  }
  def currentQuantified = flatCollect(reifyStack.currents)({ case ExistentialType(quantified, _) => quantified })
  def current = reifyStack.currents.head

  /**
   *  Reifies any supported value.
   *  For internal use only, use ``reified'' instead.
   */
  def reify(reifee: Any): Tree = reifyStack.push(reifee)(reifee match {
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
  })
}