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

    /** A fresh symbol with given name `name`, position `pos` and flags `flags` that has
     *  the current symbol as its owner.
     */
    def newNestedSymbol(owner: Symbol, name: Name, pos: Position, flags: FlagSet, isClass: Boolean): Symbol

    /** Create a fresh free term symbol.
     *  @param   name   the name of the free variable
     *  @param   info   the type signature of the free variable
     *  @param   value  the value of the free variable at runtime
     *  @param   flags  (optional) flags of the free variable
     *  @param   origin debug information that tells where this symbol comes from
     */
    def newFreeTerm(name: String, info: Type, value: => Any, flags: FlagSet = NoFlags, origin: String = null): FreeTermSymbol

    /** Create a fresh free non-existential type symbol.
     *  @param   name   the name of the free variable
     *  @param   info   the type signature of the free variable
     *  @param   value  a type tag that captures the value of the free variable
     *                  is completely phantom, since the captured type cannot be propagated to the runtime
     *                  if it could be, we wouldn't be creating a free type to begin with
     *                  the only usage for it is preserving the captured symbol for compile-time analysis
     *  @param   flags  (optional) flags of the free variable
     *  @param   origin debug information that tells where this symbol comes from
     */
    def newFreeType(name: String, info: Type, value: => Any, flags: FlagSet = NoFlags, origin: String = null): FreeTypeSymbol

    /** Create a fresh free existential type symbol.
     *  @param   name   the name of the free variable
     *  @param   info   the type signature of the free variable
     *  @param   value  a type tag that captures the value of the free variable
     *                  is completely phantom, since the captured type cannot be propagated to the runtime
     *                  if it could be, we wouldn't be creating a free type to begin with
     *                  the only usage for it is preserving the captured symbol for compile-time analysis
     *  @param   flags  (optional) flags of the free variable
     *  @param   origin (optional) debug information that tells where this symbol comes from
     *  [Martin to Eugene: why needed?]
     */
    def newFreeExistential(name: String, info: Type, value: => Any, flags: FlagSet = NoFlags, origin: String = null): FreeTypeSymbol

    /** Set symbol's type signature to given type.
     *  @return the symbol itself
     */
    def setTypeSignature[S <: Symbol](sym: S, tpe: Type): S

    /** Set symbol's annotations to given annotations `annots`.
     */
    def setAnnotations[S <: Symbol](sym: S, annots: List[AnnotationInfo]): S

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
