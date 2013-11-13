package scala
package reflect
package api

/**
 * This is an internal implementation class.
 * @groupname TreeBuilders Tree Building
 */
private[reflect] trait BuildUtils { self: Universe =>

  /** @group TreeBuilders */
  val build: BuildApi

  // this API abstracts away the functionality necessary for reification
  // it's too gimmicky and unstructured to be exposed directly in the universe
  // but we need it in a publicly available place for reification to work

  /** @group TreeBuilders */
  abstract class BuildApi {
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
     *  @param   value  the value of the free variable at runtime
     *  @param   flags  (optional) flags of the free variable
     *  @param   origin debug information that tells where this symbol comes from
     */
    def newFreeTerm(name: String, value: => Any, flags: FlagSet = NoFlags, origin: String = null): FreeTermSymbol

    /** Create a fresh free type symbol.
     *  @param   name   the name of the free variable
     *  @param   flags  (optional) flags of the free variable
     *  @param   origin debug information that tells where this symbol comes from
     */
    def newFreeType(name: String, flags: FlagSet = NoFlags, origin: String = null): FreeTypeSymbol

    /** Set symbol's type signature to given type.
     *  @return the symbol itself
     */
    def setTypeSignature[S <: Symbol](sym: S, tpe: Type): S

    /** Set symbol's annotations to given annotations `annots`.
     */
    def setAnnotations[S <: Symbol](sym: S, annots: List[Annotation]): S

    def This(sym: Symbol): Tree

    def Select(qualifier: Tree, sym: Symbol): Select

    def Ident(sym: Symbol): Ident

    def TypeTree(tp: Type): TypeTree

    def thisPrefix(sym: Symbol): Type

    def setType[T <: Tree](tree: T, tpe: Type): T

    def setSymbol[T <: Tree](tree: T, sym: Symbol): T

    def mkAnnotation(tree: Tree): Tree

    def mkAnnotation(trees: List[Tree]): List[Tree]

    def mkRefineStat(stat: Tree): Tree

    def mkRefineStat(stats: List[Tree]): List[Tree]

    def mkPackageStat(stat: Tree): Tree

    def mkPackageStat(stats: List[Tree]): List[Tree]

    def mkEarlyDef(defn: Tree): Tree

    def mkEarlyDef(defns: List[Tree]): List[Tree]

    def RefTree(qual: Tree, sym: Symbol): Tree

    def withFreshTermName[T](prefix: String)(f: TermName => T): T

    def withFreshTypeName[T](prefix: String)(f: TypeName => T): T

    val ScalaDot: ScalaDotExtractor

    trait ScalaDotExtractor {
      def apply(name: Name): Tree
      def unapply(tree: Tree): Option[Name]
    }

    val FlagsRepr: FlagsReprExtractor

    trait FlagsReprExtractor {
      def apply(value: Long): FlagSet
      def unapply(flags: Long): Some[Long]
    }

    val SyntacticTypeApplied: SyntacticTypeAppliedExtractor

    trait SyntacticTypeAppliedExtractor {
      def apply(tree: Tree, targs: List[Tree]): Tree
      def unapply(tree: Tree): Some[(Tree, List[Tree])]
    }

    val SyntacticApplied: SyntacticAppliedExtractor

    trait SyntacticAppliedExtractor {
      def apply(tree: Tree, argss: List[List[Tree]]): Tree
      def unapply(tree: Tree): Some[(Tree, List[List[Tree]])]
    }

    val SyntacticApply: SyntacticApplyExtractor

    trait SyntacticApplyExtractor {
      def apply(tree: Tree, args: List[Tree]): Tree
      def unapply(tree: Tree): Some[(Tree, List[Tree])]
    }

    val SyntacticClassDef: SyntacticClassDefExtractor

    trait SyntacticClassDefExtractor {
      def apply(mods: Modifiers, name: TypeName, tparams: List[TypeDef],
                constrMods: Modifiers, vparamss: List[List[ValDef]], earlyDefs: List[Tree],
                parents: List[Tree], selfType: ValDef, body: List[Tree]): ClassDef
      def unapply(tree: Tree): Option[(Modifiers, TypeName, List[TypeDef], Modifiers, List[List[ValDef]],
                                       List[Tree], List[Tree], ValDef, List[Tree])]
    }

    val SyntacticTraitDef: SyntacticTraitDefExtractor

    trait SyntacticTraitDefExtractor {
      def apply(mods: Modifiers, name: TypeName, tparams: List[TypeDef],
                earlyDefs: List[Tree], parents: List[Tree], selfType: ValDef, body: List[Tree]): ClassDef
      def unapply(tree: Tree): Option[(Modifiers, TypeName, List[TypeDef],
                                       List[Tree], List[Tree], ValDef, List[Tree])]
    }

    val SyntacticObjectDef: SyntacticObjectDefExtractor

    trait SyntacticObjectDefExtractor {
      def apply(mods: Modifiers, name: TermName, earlyDefs: List[Tree],
                parents: List[Tree], selfType: ValDef, body: List[Tree]): Tree
      def unapply(tree: Tree): Option[(Modifiers, TermName, List[Tree], List[Tree], ValDef, List[Tree])]
    }

    val SyntacticPackageObjectDef: SyntacticPackageObjectDefExtractor

    trait SyntacticPackageObjectDefExtractor {
      def apply(name: TermName, earlyDefs: List[Tree],
                parents: List[Tree], selfType: ValDef, body: List[Tree]): Tree
      def unapply(tree: Tree): Option[(TermName, List[Tree], List[Tree], ValDef, List[Tree])]
    }

    val SyntacticTuple: SyntacticTupleExtractor
    val SyntacticTupleType: SyntacticTupleExtractor

    trait SyntacticTupleExtractor {
      def apply(args: List[Tree]): Tree
      def unapply(tree: Tree): Option[List[Tree]]
    }

    val SyntacticBlock: SyntacticBlockExtractor

    trait SyntacticBlockExtractor {
      def apply(stats: List[Tree]): Tree
      def unapply(tree: Tree): Option[List[Tree]]
    }

    val SyntacticNew: SyntacticNewExtractor

    trait SyntacticNewExtractor {
      def apply(earlyDefs: List[Tree], parents: List[Tree], selfType: ValDef, body: List[Tree]): Tree
      def unapply(tree: Tree): Option[(List[Tree], List[Tree], ValDef, List[Tree])]
    }

    val SyntacticFunctionType: SyntacticFunctionTypeExtractor

    trait SyntacticFunctionTypeExtractor {
      def apply(argtpes: List[Tree], restpe: Tree): Tree
      def unapply(tree: Tree): Option[(List[Tree], Tree)]
    }

    val SyntacticFunction: SyntacticFunctionExtractor

    trait SyntacticFunctionExtractor {
      def apply(params: List[ValDef], body: Tree): Tree

      def unapply(tree: Tree): Option[(List[ValDef], Tree)]
    }

    val SyntacticDefDef: SyntacticDefDefExtractor

    trait SyntacticDefDefExtractor {
      def apply(mods: Modifiers, name: TermName, tparams: List[Tree], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree): DefDef

      def unapply(tree: Tree): Option[(Modifiers, TermName, List[Tree], List[List[ValDef]], Tree, Tree)]
    }

    val SyntacticValDef: SyntacticValDefExtractor
    val SyntacticVarDef: SyntacticValDefExtractor

    trait SyntacticValDefExtractor {
      def apply(mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree): ValDef
      def unapply(tree: Tree): Option[(Modifiers, TermName, Tree, Tree)]
    }

    val SyntacticAssign: SyntacticAssignExtractor

    trait SyntacticAssignExtractor {
      def apply(lhs: Tree, rhs: Tree): Tree
      def unapply(tree: Tree): Option[(Tree, Tree)]
    }

    class UnliftHelper1[T](unliftable: Unliftable[T]) {
      def unapply(lst: List[Tree]): Option[List[T]] = {
        val unlifted = lst.flatMap(unliftable.unapply(_))
        if (unlifted.length == lst.length) Some(unlifted) else None
      }
    }
    object UnliftHelper1 { def apply[T](unliftable: Unliftable[T]) = new UnliftHelper1[T](unliftable) }

    class UnliftHelper2[T](unliftable: Unliftable[T]) {
      def unapply(lst: List[List[Tree]]): Option[List[List[T]]] = {
        val unlifted = lst.map { l => l.flatMap(unliftable.unapply(_)) }
        if (unlifted.flatten.length == lst.flatten.length) Some(unlifted) else None
      }
    }
    object UnliftHelper2 { def apply[T](unliftable: Unliftable[T]) = new UnliftHelper2[T](unliftable) }
  }
}
