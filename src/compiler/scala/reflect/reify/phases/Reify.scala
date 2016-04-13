package scala.reflect.reify
package phases

import scala.reflect.reify.codegen._

trait Reify extends GenSymbols
               with GenTypes
               with GenNames
               with GenTrees
               with GenAnnotationInfos
               with GenPositions
               with GenUtils {

  self: Reifier =>

  import global._

  private object reifyStack {
    def currents: List[Any] = state.reifyStack
    def currents_=(value: List[Any]): Unit = state.reifyStack = value

    @inline final def push[T](reifee: Any)(body: => T): T = {
      currents ::= reifee
      try body
      finally currents = currents.tail
    }
  }
  def boundSymbolsInCallstack = flatCollect(reifyStack.currents) {
    case ExistentialType(quantified, _) => quantified
    case PolyType(typeParams, _) => typeParams
  }
  def current = reifyStack.currents.head
  def currents = reifyStack.currents

  /**
   *  Reifies any supported value.
   *  For internal use only, use `reified` instead.
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
    // case ann: AnnotationInfo      => reifyAnnotationInfo(ann)
    case pos: Position            => reifyPosition(pos)
    case mods: global.Modifiers   => reifyModifiers(mods)
    case xs: List[_]              => reifyList(xs)
    case s: String                => Literal(Constant(s))
    case v if isAnyVal(v)         => Literal(Constant(v))
    case null                     => Literal(Constant(null))
    case _                        =>
      throw new Error("reifee %s of type %s is not supported".format(reifee, reifee.getClass))
  })

  private def isAnyVal(x: Any) = x match {
    case _: Byte | _: Short | _: Char | _: Int | _: Long | _: Float | _: Double | _: Boolean | _: Unit => true
    case _                                                                                             => false
  }
}
