package scala.tools.nsc
package typechecker

trait Tags {
  self: Analyzer =>

  import global._
  import definitions._

  trait Tag {
    self: Typer =>

    private def resolveTag(pos: Position, taggedTp: Type, allowMaterialization: Boolean) = beforeTyper {
      def wrapper (tree: => Tree): Tree = if (allowMaterialization) (context.withMacrosEnabled[Tree](tree)) else (context.withMacrosDisabled[Tree](tree))
      wrapper(inferImplicit(
        EmptyTree,
        taggedTp,
        /*reportAmbiguous =*/ true,
        /*isView =*/ false,
        /*context =*/ context,
        /*saveAmbiguousDivergent =*/ true,
        /*pos =*/ pos
      ).tree)
    }

    /** Finds in scope or materializes a ClassTag.
     *  Should be used instead of ClassManifest every time compiler needs to persist an erasure.
     *
     *  Once upon a time, we had an `ErasureTag` which was to `ClassTag` the same that `AbsTypeTag` is for `TypeTag`.
     *  However we found out that we don't really need this concept, so it got removed.
     *
     *  @param   pos                    Position for error reporting. Please, provide meaningful value.
     *  @param   tp                     Type we're looking a ClassTag for, e.g. resolveClassTag(pos, IntClass.tpe) will look for ClassTag[Int].
     *  @param   allowMaterialization   If true (default) then the resolver is allowed to launch materialization macros when there's no class tag in scope.
     *                                  If false then materialization macros are prohibited from running.
     *
     *  @returns Tree that represents an `scala.reflect.ClassTag` for `tp` if everything is okay.
     *           EmptyTree if the result contains unresolved (i.e. not spliced) type parameters and abstract type members.
     *           EmptyTree if `allowMaterialization` is false, and there is no class tag in scope.
     */
    def resolveClassTag(pos: Position, tp: Type, allowMaterialization: Boolean = true): Tree = {
      val taggedTp = appliedType(ClassTagClass.typeConstructor, List(tp))
      resolveTag(pos, taggedTp, allowMaterialization)
    }

    /** Finds in scope or materializes an AbsTypeTag (if `concrete` is false) or a TypeTag (if `concrete` is true).
     *
     *  @param   pos                    Position for error reporting. Please, provide meaningful value.
     *  @param   pre                    Prefix that represents a universe this type tag will be bound to.
     *                                  If `pre` is set to `NoType`, then any type tag in scope will do, regardless of its affiliation.
     *                                  If `pre` is set to `NoType`, and tag resolution involves materialization, then `mkBasisPrefix` will be used.
     *  @param   tp                     Type we're looking a TypeTag for, e.g. resolveTypeTag(pos, reflectBasisPrefix, IntClass.tpe, false) will look for scala.reflect.basis.TypeTag[Int].
     *  @param   concrete               If true then the result must not contain unresolved (i.e. not spliced) type parameters and abstract type members.
     *                                  If false then the function will always succeed (abstract types will be reified as free types).
     *  @param   allowMaterialization   If true (default) then the resolver is allowed to launch materialization macros when there's no type tag in scope.
     *                                  If false then materialization macros are prohibited from running.
     *
     *  @returns Tree that represents a `scala.reflect.TypeTag` for `tp` if everything is okay.
     *           EmptyTree if `concrete` is true and the result contains unresolved (i.e. not spliced) type parameters and abstract type members.
     *           EmptyTree if `allowMaterialization` is false, and there is no array tag in scope.
     */
    def resolveTypeTag(pos: Position, pre: Type, tp: Type, concrete: Boolean, allowMaterialization: Boolean = true): Tree = {
      val tagSym = if (concrete) TypeTagClass else AbsTypeTagClass
      val tagTp =  if (pre == NoType) TypeRef(BaseUniverseClass.asTypeConstructor, tagSym, List(tp)) else singleType(pre, pre member tagSym.name)
      val taggedTp = appliedType(tagTp, List(tp))
      resolveTag(pos, taggedTp, allowMaterialization)
    }
  }
}