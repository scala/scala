package scala.reflect
package base

trait BuildUtils { self: Universe =>

  val build: BuildBase

  abstract class BuildBase {
    /** Selects type symbol with given simple name `name` from the defined members of `owner`.
     */
    def selectType(owner: Symbol, name: String): TypeSymbol

    /** Selects term symbol with given name and type from the defined members of prefix type
     */
    def selectTerm(owner: Symbol, name: String): TermSymbol

    /** Selects overloaded method symbol with given name and index
     */
    def selectOverloadedMethod(owner: Symbol, name: String, index: Int): MethodSymbol

    /** A fresh symbol with given name `name`, position `pos` and flags `flags` that has the current symbol as its owner.
     *
     *  If the optional `protoid` argument is not 0, and the owner already contains a child symbol with this `protoid`
     *  then the already existing child symbol will be used instead of creating a new one.
     */
    def newNestedSymbol(owner: Symbol, name: Name, pos: Position, flags: FlagSet, isClass: Boolean, protoid: Int = 0): Symbol

    /** Create a fresh free term symbol, possibly reusing a preexisting one.
     *
     *  @param   name      the name of the free variable
     *  @param   value     the value of the free variable at runtime
     *  @param   flags     (optional) flags of the free variable
     *  @param   origin    (optional) debug information that tells where this symbol comes from
     *  @param   protoid   (optional) unique id of the free variable's prototype
     *                     if `protoid` is not 0, and the universe already contains a free variable with this `protoid`
     *                     then the already existing free variable will be used instead of creating a new one
     */
    def newFreeTerm(name: String, value: => Any, flags: FlagSet = NoFlags, origin: String = null, protoid: Int = 0): FreeTermSymbol

    /** Create a fresh free non-existential type symbol, possibly reusing a preexisting one.
     *
     *  @param   name      the name of the free variable
     *  @param   value     (optional) a type tag that captures the value of the free variable
     *                     is completely phantom, since the captured type cannot be propagated to the runtime
     *                     if it could be, we wouldn't be creating a free type to begin with
     *                     the only usage for it is preserving the captured symbol for compile-time analysis
     *  @param   flags     (optional) flags of the free variable
     *  @param   origin    (optional) debug information that tells where this symbol comes from
     *  @param   protoid   (optional) unique id of the free variable
     *                     if `protoid` is not 0, and the universe already contains a free variable with this `protoid`
     *                     then the already existing free variable will be used instead of creating a new one
     */
    def newFreeType(name: String, value: => Any = null, flags: FlagSet = NoFlags, origin: String = null, protoid: Int = 0): FreeTypeSymbol

    /** Create a fresh free existential type symbol, possibly reusing a preexisting one.
     *
     *  @param   name      the name of the free variable
     *  @param   value     (optional) a type tag that captures the value of the free variable
     *                     is completely phantom, since the captured type cannot be propagated to the runtime
     *                     if it could be, we wouldn't be creating a free type to begin with
     *                     the only usage for it is preserving the captured symbol for compile-time analysis
     *  @param   flags     (optional) flags of the free variable
     *  @param   origin    (optional) debug information that tells where this symbol comes from
     *  @param   protoid   (optional) unique id of the free variable
     *                     if `protoid` is not 0, and the universe already contains a free variable with this `protoid`
     *                     then the already existing free variable will be used instead of creating a new one
     *
     *  [Martin to Eugene: why needed?]
     *  [Eugene to Martin] to distinguish free types (that make a type tag non-concrete) and existentials (that just stand for ExistentialTypes' tparams)
     */
    def newFreeExistential(name: String, value: => Any = null, flags: FlagSet = NoFlags, origin: String = null, protoid: Int = 0): FreeTypeSymbol

    /** Set symbol's type signature to given type.
     *  @return the symbol itself
     */
    def setTypeSignature[S <: Symbol](sym: S, tpe: Type): S

    /** If symbol doesn't yet have a type signautre, set symbol's type signature to given type.
     *  Otherwise do nothing.
     *  @return the symbol itself
     */
    def setTypeSignatureIfEmpty[S <: Symbol](sym: S, tpe: Type): S

    /** Set symbol's annotations to given annotations `annots`.
     *  @return the symbol itself
     */
    def setAnnotations[S <: Symbol](sym: S, annots: List[AnnotationInfo]): S

    /** If symbol doesn't yet have annotations assigned, set symbol's annotations to given annotations `annots`.
     *  Otherwise do nothing.
     *  @return the symbol itself
     */
    def setAnnotationsIfEmpty[S <: Symbol](sym: S, annots: List[AnnotationInfo]): S

    def flagsFromBits(bits: Long): FlagSet

    // [Eugene++ to Martin] these are necessary for reification
    // on a second thought, I added them to BuildUtils instead of base

    def emptyValDef: ValDef

    def This(sym: Symbol): Tree

    def Select(qualifier: Tree, sym: Symbol): Select

    def Ident(sym: Symbol): Ident

    def TypeTree(tp: Type): TypeTree

    def thisPrefix(sym: Symbol): Type

    def setType[T <: Tree](tree: T, tpe: Type): T

    def setSymbol[T <: Tree](tree: T, sym: Symbol): T
  }
}
