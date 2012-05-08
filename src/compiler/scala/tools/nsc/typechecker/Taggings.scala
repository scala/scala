package scala.tools.nsc
package typechecker

trait Taggings {
  self: Analyzer =>

  import global._
  import definitions._

  trait Tagging {
    self: Typer =>

    private def resolveTag(taggedTp: Type, pos: Position) = beforeTyper {
      inferImplicit(
        EmptyTree,
        taggedTp,
        /*reportAmbiguous =*/ true,
        /*isView =*/ false,
        /*context =*/ context,
        /*saveAmbiguousDivergent =*/ true,
        /*pos =*/ pos
      ).tree
    }

    /** Finds in scope or materializes an ArrayTag.
     *  Should be used instead of ClassTag or ClassManifest every time compiler needs to create an array.
     *
     *  @param   tp         Type we're looking an ArrayTag for, e.g. resolveArrayTag(IntClass.tpe, pos) will look for ArrayTag[Int].
     *  @param   pos        Position for error reporting. Please, provide meaningful value.
     *
     *  @returns Tree that represents an `scala.reflect.ArrayTag` for `tp` if everything is okay.
     *           EmptyTree if the result contains unresolved (i.e. not spliced) type parameters and abstract type members.
     */
    def resolveArrayTag(tp: Type, pos: Position): Tree = {
      val taggedTp = appliedType(ArrayTagClass.typeConstructor, List(tp))
      resolveTag(taggedTp, pos)
    }

    /** Finds in scope or materializes an ErasureTag (if `concrete` is false) or a ClassTag (if `concrete` is true).
     *  Should be used instead of ClassTag or ClassManifest every time compiler needs to persist an erasure.
     *
     *  @param   tp         Type we're looking an ErasureTag for, e.g. resolveErasureTag(IntClass.tpe, pos, true) will look for ClassTag[Int].
     *  @param   pos        Position for error reporting. Please, provide meaningful value.
     *  @param   concrete   If true then the result must not contain unresolved (i.e. not spliced) type parameters and abstract type members.
     *                      If false then the function will always succeed (abstract types will be erased to their upper bounds).
     *
     *  @returns Tree that represents an `scala.reflect.ErasureTag` for `tp` if everything is okay.
     *           EmptyTree if `concrete` is true and the result contains unresolved (i.e. not spliced) type parameters and abstract type members.
     */
    def resolveErasureTag(tp: Type, pos: Position, concrete: Boolean): Tree = {
      val taggedTp = appliedType(if (concrete) ClassTagClass.typeConstructor else ErasureTagClass.typeConstructor, List(tp))
      resolveTag(taggedTp, pos)
    }

    /** Finds in scope or materializes a TypeTag (if `concrete` is false) or a ConcreteTypeTag (if `concrete` is true).
     *
     *  @param   pre        Prefix that represents a universe this type tag will be bound to.
     *  @param   tp         Type we're looking a TypeTag for, e.g. resolveTypeTag(reflectMirrorPrefix, IntClass.tpe, pos, false) will look for scala.reflect.mirror.TypeTag[Int].
     *  @param   pos        Position for error reporting. Please, provide meaningful value.
     *  @param   concrete   If true then the result must not contain unresolved (i.e. not spliced) type parameters and abstract type members.
     *                      If false then the function will always succeed (abstract types will be reified as free types).
     *
     *  @returns Tree that represents a `scala.reflect.TypeTag` for `tp` if everything is okay.
     *           EmptyTree if `concrete` is true and the result contains unresolved (i.e. not spliced) type parameters and abstract type members.
     */
    def resolveTypeTag(pre: Type, tp: Type, pos: Position, concrete: Boolean): Tree = {
      val taggedTp = appliedType(singleType(pre, pre member (if (concrete) ConcreteTypeTagClass else TypeTagClass).name), List(tp))
      resolveTag(taggedTp, pos)
    }
  }
}