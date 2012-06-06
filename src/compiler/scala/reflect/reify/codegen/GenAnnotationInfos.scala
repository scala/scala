package scala.reflect.reify
package codegen

trait GenAnnotationInfos {
  self: Reifier =>

  import global._
  import definitions._

  // usually annotations are reified as their originals from Modifiers
  // however, when reifying free and tough types, we're forced to reify annotation infos as is
  // why is that bad? take a look inside
  def reifyAnnotationInfo(ann: AnnotationInfo): Tree = {
    val reifiedArgs = ann.args map { arg =>
      val saved1 = reifyTreeSymbols
      val saved2 = reifyTreeTypes

      try {
        // one more quirk of reifying annotations
        //
        // when reifying AnnotatedTypes we need to reify all the types and symbols of inner ASTs
        // that's because a lot of logic expects post-typer trees to have non-null tpes
        //
        // Q: reified trees are pre-typer, so there's shouldn't be a problem.
        //    reflective typechecker will fill in missing symbols and types, right?
        // A: actually, no. annotation ASTs live inside AnnotatedTypes,
        //    and insides of the types is the place where typechecker doesn't look.
        state.reifyTreeSymbols = true
        state.reifyTreeTypes = true

        // todo. every AnnotationInfo is an island, entire of itself
        // no regular Traverser or Transformer can reach it
        // hence we need to run its contents through the entire reification pipeline
        // e.g. to apply reshaping or to check metalevels
        reify(arg)
      } finally {
        state.reifyTreeSymbols = saved1
        state.reifyTreeTypes = saved2
      }
    }

    def reifyClassfileAnnotArg(arg: ClassfileAnnotArg): Tree = arg match {
      case LiteralAnnotArg(const) =>
        mirrorFactoryCall(nme.LiteralAnnotArg, reifyProduct(const))
      case ArrayAnnotArg(args) =>
        mirrorFactoryCall(nme.ArrayAnnotArg, scalaFactoryCall(nme.Array, args map reifyClassfileAnnotArg: _*))
      case NestedAnnotArg(ann) =>
        mirrorFactoryCall(nme.NestedAnnotArg, reifyAnnotationInfo(ann))
    }

    // if you reify originals of anns, you get SO when trying to reify AnnotatedTypes, so screw it - after all, it's not that important
    val reifiedAssocs = ann.assocs map (assoc => scalaFactoryCall(nme.Tuple2, reify(assoc._1), reifyClassfileAnnotArg(assoc._2)))
    mirrorFactoryCall(nme.AnnotationInfo, reify(ann.atp), mkList(reifiedArgs), mkList(reifiedAssocs))
  }
}