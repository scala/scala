package scala.tools.nsc
package transform

trait PostErasure extends InfoTransform with TypingTransformers {

  val global: Global
  import global._
  import definitions._

  val phaseName: String = "posterasure"

  def newTransformer(unit: CompilationUnit): Transformer = new PostErasureTransformer(unit)
  override def changesBaseClasses = false

  object elimErasedInline extends TypeMap {
    def apply(tp: Type) = tp match {
      case ErasedInlineType(clazz) => erasure.valueClassErasure(clazz)
      case _ => mapOver(tp)
    }
  }

  def transformInfo(sym: Symbol, tp: Type) = elimErasedInline(tp)

  class PostErasureTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    override def transform(tree: Tree) =
      super.transform(tree) setType elimErasedInline(tree.tpe) match {
        case // new C(arg).underlying  ==>  arg
          Apply(sel @ Select(
            Apply(Select(New(tpt), nme.CONSTRUCTOR), List(arg)),
            acc), List())
        if atPhase(currentRun.erasurePhase) {
          tpt.tpe.typeSymbol.isInlineClass &&
          sel.symbol == tpt.tpe.typeSymbol.firstParamAccessor
        } =>
          if (settings.debug.value) log("Removing "+tree+" -> "+arg)
          arg
        case // new C(arg1) == new C(arg2)  ==>  arg1 == arg2
          Apply(sel @ Select(
            Apply(Select(New(tpt1), nme.CONSTRUCTOR), List(arg1)),
            cmp),
            List(Apply(Select(New(tpt2), nme.CONSTRUCTOR), List(arg2))))
        if atPhase(currentRun.erasurePhase) {
          tpt1.tpe.typeSymbol.isInlineClass &&
          (cmp == nme.EQ || cmp == nme.NE) &&
          tpt2.tpe.typeSymbol == tpt1.tpe.typeSymbol
        } =>
          val result = Apply(Select(arg1, cmp) setPos sel.pos, List(arg2)) setPos tree.pos
          log("shortcircuiting equality "+tree+" -> "+result)
          localTyper.typed(result)

        case // arg.asInstanceOf[T]  ==>  arg      if arg.tpe == T
          Apply(TypeApply(cast @ Select(arg, asinstanceof), List(tpt)), List())
        if cast.symbol == Object_asInstanceOf && arg.tpe =:= tpt.tpe => // !!! <:< ?
          if (settings.debug.value) log("Shortening "+tree+" -> "+arg)
          arg
        case tree1 =>
          tree1
      }
  }
}