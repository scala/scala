package scala
package reflect
package api

import scala.language.implicitConversions

/**
 *  <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  This trait assembles APIs occasionally necessary for performing low-level operations on reflection artifacts.
 *  See [[Internals#InternalApi]] for more information about nature, usefulness and compatibility guarantees of these APIs.
 *
 *  @group ReflectionAPI
 */
trait Internals { self: Universe =>

  /** @see [[InternalApi]]
   *  @group Internal
   */
  val internal: Internal

  /** @see [[InternalApi]]
   *  @group Internal
   */
  type Internal <: InternalApi

  /** Reflection API exhibits a tension inherent to experimental things:
   *  on the one hand we want it to grow into a beautiful and robust API,
   *  but on the other hand we have to deal with immaturity of underlying mechanisms
   *  by providing not very pretty solutions to enable important use cases.
   *
   *  In Scala 2.10, which was our first stab at reflection API, we didn't have a systematic
   *  approach to dealing with this tension, sometimes exposing too much of internals (e.g. Symbol.deSkolemize)
   *  and sometimes exposing too little (e.g. there's still no facility to change owners, to do typing
   *  transformations, etc). This resulted in certain confusion with some internal APIs
   *  living among public ones, scaring the newcomers, and some internal APIs only available via casting,
   *  which requires intimate knowledge of the compiler and breaks compatibility guarantees.
   *
   *  This led to creation of the `internal` API module for the reflection API, which
   *  provides advanced APIs necessary for macros that push boundaries of the state of the art,
   *  clearly demarcating them from the more or less straightforward rest and
   *  providing compatibility guarantees on par with the rest of the reflection API
   *  (full compatibility within minor releases, best effort towards backward compatibility within major releases,
   *  clear replacement path in case of rare incompatible changes in major releases).
   *
   *  The `internal` module itself (the value that implements [[InternalApi]]) isn't defined here,
   *  in [[scala.reflect.api.Universe]], but is provided on per-implementation basis. Runtime API endpoint
   *  ([[scala.reflect.runtime.universe]]) provides `universe.compat: InternalApi`, whereas compile-time API endpoints
   *  (instances of [[scala.reflect.macros.Context]]) provide `c.compat: ContextInternalApi`, which extends `InternalApi`
   *  with additional universe-specific and context-specific functionality.
   *
   *  @group Internal
   */
  trait InternalApi {
    /** This is an internal implementation module.
     */
    val reificationSupport: ReificationSupportApi

    /** Creates an importer that moves reflection artifacts between universes.
     *  @see [[Importer]]
     */
    // SI-6241: move importers to a mirror
    def createImporter(from0: Universe): Importer { val from: from0.type }

    /**
     * Convert a [[scala.reflect.api.TypeTags#TypeTag]] to a [[scala.reflect.Manifest]].
     *
     * Compiler usually generates these conversions automatically, when a type tag for a type `T` is in scope,
     * and an implicit of type `Manifest[T]` is requested, but this method can also be called manually.
     * For example:
     * {{{
     * typeTagToManifest(scala.reflect.runtime.currentMirror, implicitly[TypeTag[String]])
     * }}}
     * @group TagInterop
     */
    def typeTagToManifest[T: ClassTag](mirror: Any, tag: Universe#TypeTag[T]): Manifest[T] =
      throw new UnsupportedOperationException("This universe does not support tag -> manifest conversions. Use a JavaUniverse, e.g. the scala.reflect.runtime.universe.")

    /**
     * Convert a [[scala.reflect.Manifest]] to a [[scala.reflect.api.TypeTags#TypeTag]].
     *
     * Compiler usually generates these conversions automatically, when a manifest for a type `T` is in scope,
     * and an implicit of type `TypeTag[T]` is requested, but this method can also be called manually.
     * For example:
     * {{{
     * manifestToTypeTag(scala.reflect.runtime.currentMirror, implicitly[Manifest[String]])
     * }}}
     * @group TagInterop
     */
    def manifestToTypeTag[T](mirror: Any, manifest: Manifest[T]): Universe#TypeTag[T] =
      throw new UnsupportedOperationException("This universe does not support manifest -> tag conversions. Use a JavaUniverse, e.g. the scala.reflect.runtime.universe.")

    /** Create a new scope with the given initial elements.
     *  @group Scopes
     */
    def newScopeWith(elems: Symbol*): Scope

    /** Extracts free term symbols from a tree that is reified or contains reified subtrees.
     */
    def freeTerms(tree: Tree): List[FreeTermSymbol]

    /** Extracts free type symbols from a tree that is reified or contains reified subtrees.
     */
    def freeTypes(tree: Tree): List[FreeTypeSymbol]

    /** Substitute symbols in `to` for corresponding occurrences of references to
     *  symbols `from` in this type.
     */
    def substituteSymbols(tree: Tree, from: List[Symbol], to: List[Symbol]): Tree

    /** Substitute types in `to` for corresponding occurrences of references to
     *  symbols `from` in this tree.
     */
    def substituteTypes(tree: Tree, from: List[Symbol], to: List[Type]): Tree

    /** Substitute given tree `to` for occurrences of nodes that represent
     *  `C.this`, where `C` referes to the given class `clazz`.
     */
    def substituteThis(tree: Tree, clazz: Symbol, to: Tree): Tree

    /** A factory method for `ClassDef` nodes.
     */
    def classDef(sym: Symbol, impl: Template): ClassDef

    /** A factory method for `ModuleDef` nodes.
     */
    def moduleDef(sym: Symbol, impl: Template): ModuleDef

    /** A factory method for `ValDef` nodes.
     */
    def valDef(sym: Symbol, rhs: Tree): ValDef

    /** A factory method for `ValDef` nodes.
     */
    def valDef(sym: Symbol): ValDef

    /** A factory method for `DefDef` nodes.
     */
    def defDef(sym: Symbol, mods: Modifiers, vparamss: List[List[ValDef]], rhs: Tree): DefDef

    /** A factory method for `DefDef` nodes.
     */
    def defDef(sym: Symbol, vparamss: List[List[ValDef]], rhs: Tree): DefDef

    /** A factory method for `DefDef` nodes.
     */
    def defDef(sym: Symbol, mods: Modifiers, rhs: Tree): DefDef

    /** A factory method for `DefDef` nodes.
     */
    def defDef(sym: Symbol, rhs: Tree): DefDef

    /** A factory method for `DefDef` nodes.
     */
    def defDef(sym: Symbol, rhs: List[List[Symbol]] => Tree): DefDef

    /** A factory method for `TypeDef` nodes.
     */
    def typeDef(sym: Symbol, rhs: Tree): TypeDef

    /** A factory method for `TypeDef` nodes.
     */
    def typeDef(sym: Symbol): TypeDef

    /** A factory method for `LabelDef` nodes.
     */
    def labelDef(sym: Symbol, params: List[Symbol], rhs: Tree): LabelDef

    /** Does this symbol represent a free term captured by reification?
     *  If yes, `isTerm` is also guaranteed to be true.
     */
    def isFreeTerm(symbol: Symbol): Boolean

    /** This symbol cast to a free term symbol.
     *  @throws ScalaReflectionException if `isFreeTerm` is false.
     */
    def asFreeTerm(symbol: Symbol): FreeTermSymbol

    /** Does this symbol represent a free type captured by reification?
     *  If yes, `isType` is also guaranteed to be true.
     */
    def isFreeType(symbol: Symbol): Boolean

    /** This symbol cast to a free type symbol.
     *  @throws ScalaReflectionException if `isFreeType` is false.
     */
    def asFreeType(symbol: Symbol): FreeTypeSymbol

    def newTermSymbol(owner: Symbol, name: TermName, pos: Position = NoPosition, flags: FlagSet = NoFlags): TermSymbol

    def newModuleAndClassSymbol(owner: Symbol, name: Name, pos: Position = NoPosition, flags: FlagSet = NoFlags): (ModuleSymbol, ClassSymbol)

    def newMethodSymbol(owner: Symbol, name: TermName, pos: Position = NoPosition, flags: FlagSet = NoFlags): MethodSymbol

    def newTypeSymbol(owner: Symbol, name: TypeName, pos: Position = NoPosition, flags: FlagSet = NoFlags): TypeSymbol

    def newClassSymbol(owner: Symbol, name: TypeName, pos: Position = NoPosition, flags: FlagSet = NoFlags): ClassSymbol

    def newFreeTerm(name: String, value: => Any, flags: FlagSet = NoFlags, origin: String = null): FreeTermSymbol

    def newFreeType(name: String, flags: FlagSet = NoFlags, origin: String = null): FreeTypeSymbol

    /** Does this symbol or its underlying type represent a typechecking error?
     */
    def isErroneous(symbol: Symbol): Boolean

    /** Does this symbol represent the definition of a skolem?
     *  Skolems are used during typechecking to represent type parameters viewed from inside their scopes.
     */
    def isSkolem(symbol: Symbol): Boolean

    /** If this symbol is a skolem, its corresponding type parameter, otherwise the symbol itself.
     *
     *  [[https://groups.google.com/forum/#!msg/scala-internals/0j8laVNTQsI/kRXMF_c8bGsJ To quote Martin Odersky]],
     *  skolems are synthetic type "constants" that are copies of existentially bound or universally
     *  bound type variables. E.g. if one is inside the right-hand side of a method:
     *
     *  {{{
     *  def foo[T](x: T) = ... foo[List[T]]....
     *  }}}
     *
     *  the skolem named `T` refers to the unknown type instance of `T` when `foo` is called. It needs to be different
     *  from the type parameter because in a recursive call as in the `foo[List[T]]` above the type parameter gets
     *  substituted with `List[T]`, but the ''type skolem'' stays what it is.
     *
     *  The other form of skolem is an ''existential skolem''. Say one has a function
     *
     *  {{{
     *  def bar(xs: List[T] forSome { type T }) = xs.head
     *  }}}
     *
     *  then each occurrence of `xs` on the right will have type `List[T']` where `T'` is a fresh copy of `T`.
     */
    def deSkolemize(symbol: Symbol): Symbol

    /** Forces all outstanding completers associated with this symbol.
     *  After this call returns, the symbol becomes immutable and thread-safe.
     */
    def initialize(symbol: Symbol): symbol.type

    /** Calls [[initialize]] on the owner and all the value and type parameters of the symbol.
     */
    def fullyInitialize(symbol: Symbol): symbol.type

    /** Calls [[initialize]] on all the value and type parameters of the type.
     */
    def fullyInitialize(tp: Type): tp.type

    /** Calls [[initialize]] on all the symbols that the scope consists of.
     */
    def fullyInitialize(scope: Scope): scope.type

    /** A creator for `ThisType` types.
     */
    def thisType(sym: Symbol): Type

    /** A creator for `SingleType` types.
     */
    def singleType(pre: Type, sym: Symbol): Type

    /** A creator for `SuperType` types.
     */
    def superType(thistpe: Type, supertpe: Type): Type

    /** A creator for `ConstantType` types.
     */
    def constantType(value: Constant): ConstantType

    /** A creator for `TypeRef` types.
     */
    def typeRef(pre: Type, sym: Symbol, args: List[Type]): Type

    /** A creator for `RefinedType` types.
     */
    def refinedType(parents: List[Type], decls: Scope): RefinedType

    /** A creator for `RefinedType` types.
     */
    def refinedType(parents: List[Type], owner: Symbol): Type

    /** A creator for `RefinedType` types.
     */
    def refinedType(parents: List[Type], owner: Symbol, decls: Scope): Type

    /** A creator for `RefinedType` types.
     */
    def refinedType(parents: List[Type], owner: Symbol, decls: Scope, pos: Position): Type

    /** A creator for intersection type where intersections of a single type are
     *  replaced by the type itself.
     */
    def intersectionType(tps: List[Type]): Type

    /** A creator for intersection type where intersections of a single type are
     *  replaced by the type itself, and repeated parent classes are merged.
     *
     *  !!! Repeated parent classes are not merged - is this a bug in the
     *  comment or in the code?
     */
    def intersectionType(tps: List[Type], owner: Symbol): Type

    /** A creator for `ClassInfoType` types.
     */
    def classInfoType(parents: List[Type], decls: Scope, typeSymbol: Symbol): ClassInfoType

    /** A creator for `MethodType` types.
     */
    def methodType(params: List[Symbol], resultType: Type): MethodType

    /** A creator for `NullaryMethodType` types.
     */
    def nullaryMethodType(resultType: Type): NullaryMethodType

    /** A creator for type parameterizations that strips empty type parameter lists.
     *  Use this factory method to indicate the type has kind * (it's a polymorphic value)
     *  until we start tracking explicit kinds equivalent to typeFun (except that the latter requires tparams nonEmpty).
     */
    def polyType(tparams: List[Symbol], tpe: Type): PolyType

    /** A creator for `ExistentialType` types.
     */
    def existentialType(quantified: List[Symbol], underlying: Type): ExistentialType

    /** A creator for existential types. This generates:
     *
     *  {{{
     *    tpe1 where { tparams }
     *  }}}
     *
     *  where `tpe1` is the result of extrapolating `tpe` with regard to `tparams`.
     *  Extrapolating means that type variables in `tparams` occurring
     *  in covariant positions are replaced by upper bounds, (minus any
     *  SingletonClass markers), type variables in `tparams` occurring in
     *  contravariant positions are replaced by upper bounds, provided the
     *  resulting type is legal with regard to stability, and does not contain
     *  any type variable in `tparams`.
     *
     *  The abstraction drops all type parameters that are not directly or
     *  indirectly referenced by type `tpe1`. If there are no remaining type
     *  parameters, simply returns result type `tpe`.
     *  @group TypeCreators
     */
    def existentialAbstraction(tparams: List[Symbol], tpe0: Type): Type

    /** A creator for `AnnotatedType` types.
     */
    def annotatedType(annotations: List[Annotation], underlying: Type): AnnotatedType

    /** A creator for `TypeBounds` types.
     */
    def typeBounds(lo: Type, hi: Type): TypeBounds

    /** A creator for `BoundedWildcardType` types.
     */
    def boundedWildcardType(bounds: TypeBounds): BoundedWildcardType
  }

  /** This is an internal implementation class.
   *  @group Internal
   */
  // this API abstracts away the functionality necessary for reification and quasiquotes
  // it's too gimmicky and unstructured to be exposed directly in the universe
  // but we need it in a publicly available place for reification to work
  trait ReificationSupportApi {
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

    def newScopeWith(elems: Symbol*): Scope

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

    def ThisType(sym: Symbol): Type

    def SingleType(pre: Type, sym: Symbol): Type

    def SuperType(thistpe: Type, supertpe: Type): Type

    def ConstantType(value: Constant): ConstantType

    def TypeRef(pre: Type, sym: Symbol, args: List[Type]): Type

    def RefinedType(parents: List[Type], decls: Scope, typeSymbol: Symbol): RefinedType

    def ClassInfoType(parents: List[Type], decls: Scope, typeSymbol: Symbol): ClassInfoType

    def MethodType(params: List[Symbol], resultType: Type): MethodType

    def NullaryMethodType(resultType: Type): NullaryMethodType

    def PolyType(typeParams: List[Symbol], resultType: Type): PolyType

    def ExistentialType(quantified: List[Symbol], underlying: Type): ExistentialType

    def AnnotatedType(annotations: List[Annotation], underlying: Type): AnnotatedType

    def TypeBounds(lo: Type, hi: Type): TypeBounds

    def BoundedWildcardType(bounds: TypeBounds): BoundedWildcardType

    def thisPrefix(sym: Symbol): Type

    def setType[T <: Tree](tree: T, tpe: Type): T

    def setSymbol[T <: Tree](tree: T, sym: Symbol): T

    def toStats(tree: Tree): List[Tree]

    def mkAnnotation(tree: Tree): Tree

    def mkAnnotation(trees: List[Tree]): List[Tree]

    def mkRefineStat(stat: Tree): Tree

    def mkRefineStat(stats: List[Tree]): List[Tree]

    def mkPackageStat(stat: Tree): Tree

    def mkPackageStat(stats: List[Tree]): List[Tree]

    def mkEarlyDef(defn: Tree): Tree

    def mkEarlyDef(defns: List[Tree]): List[Tree]

    def RefTree(qual: Tree, sym: Symbol): Tree

    def freshTermName(prefix: String): TermName

    def freshTypeName(prefix: String): TypeName

    val ImplicitParams: ImplicitParamsExtractor

    trait ImplicitParamsExtractor {
      def apply(paramss: List[List[ValDef]], implparams: List[ValDef]): List[List[ValDef]]
      def unapply(vparamss: List[List[ValDef]]): Some[(List[List[ValDef]], List[ValDef])]
    }

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

    val SyntacticClassDef: SyntacticClassDefExtractor

    trait SyntacticClassDefExtractor {
      def apply(mods: Modifiers, name: TypeName, tparams: List[Tree],
                constrMods: Modifiers, vparamss: List[List[Tree]],
                earlyDefs: List[Tree], parents: List[Tree], selfType: Tree, body: List[Tree]): ClassDef
      def unapply(tree: Tree): Option[(Modifiers, TypeName, List[TypeDef], Modifiers, List[List[ValDef]],
                                       List[Tree], List[Tree], ValDef, List[Tree])]
    }

    val SyntacticTraitDef: SyntacticTraitDefExtractor

    trait SyntacticTraitDefExtractor {
      def apply(mods: Modifiers, name: TypeName, tparams: List[Tree],
                earlyDefs: List[Tree], parents: List[Tree], selfType: Tree, body: List[Tree]): ClassDef
      def unapply(tree: Tree): Option[(Modifiers, TypeName, List[TypeDef],
                                       List[Tree], List[Tree], ValDef, List[Tree])]
    }

    val SyntacticObjectDef: SyntacticObjectDefExtractor

    trait SyntacticObjectDefExtractor {
      def apply(mods: Modifiers, name: TermName, earlyDefs: List[Tree],
                parents: List[Tree], selfType: Tree, body: List[Tree]): ModuleDef
      def unapply(tree: Tree): Option[(Modifiers, TermName, List[Tree], List[Tree], ValDef, List[Tree])]
    }

    val SyntacticPackageObjectDef: SyntacticPackageObjectDefExtractor

    trait SyntacticPackageObjectDefExtractor {
      def apply(name: TermName, earlyDefs: List[Tree],
                parents: List[Tree], selfType: Tree, body: List[Tree]): PackageDef
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
      def apply(earlyDefs: List[Tree], parents: List[Tree], selfType: Tree, body: List[Tree]): Tree
      def unapply(tree: Tree): Option[(List[Tree], List[Tree], ValDef, List[Tree])]
    }

    val SyntacticFunctionType: SyntacticFunctionTypeExtractor

    trait SyntacticFunctionTypeExtractor {
      def apply(argtpes: List[Tree], restpe: Tree): Tree
      def unapply(tree: Tree): Option[(List[Tree], Tree)]
    }

    val SyntacticFunction: SyntacticFunctionExtractor

    trait SyntacticFunctionExtractor {
      def apply(params: List[Tree], body: Tree): Function

      def unapply(tree: Function): Option[(List[ValDef], Tree)]
    }

    val SyntacticDefDef: SyntacticDefDefExtractor

    trait SyntacticDefDefExtractor {
      def apply(mods: Modifiers, name: TermName, tparams: List[Tree],
                vparamss: List[List[Tree]], tpt: Tree, rhs: Tree): DefDef

      def unapply(tree: Tree): Option[(Modifiers, TermName, List[TypeDef], List[List[ValDef]], Tree, Tree)]
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

    val SyntacticValFrom: SyntacticValFromExtractor

    trait SyntacticValFromExtractor {
      def apply(pat: Tree, rhs: Tree): Tree
      def unapply(tree: Tree): Option[(Tree, Tree)]
    }

    val SyntacticValEq: SyntacticValEqExtractor

    trait SyntacticValEqExtractor {
      def apply(pat: Tree, rhs: Tree): Tree
      def unapply(tree: Tree): Option[(Tree, Tree)]
    }

    val SyntacticFilter: SyntacticFilterExtractor

    trait SyntacticFilterExtractor {
      def apply(test: Tree): Tree
      def unapply(tree: Tree): Option[(Tree)]
    }

    val SyntacticEmptyTypeTree: SyntacticEmptyTypeTreeExtractor

    trait SyntacticEmptyTypeTreeExtractor {
      def apply(): TypeTree
      def unapply(tt: TypeTree): Boolean
    }

    val SyntacticFor: SyntacticForExtractor
    val SyntacticForYield: SyntacticForExtractor

    trait SyntacticForExtractor {
      def apply(enums: List[Tree], body: Tree): Tree
      def unapply(tree: Tree): Option[(List[Tree], Tree)]
    }

    def UnliftListElementwise[T](unliftable: Unliftable[T]): UnliftListElementwise[T]
    trait UnliftListElementwise[T] {
      def unapply(lst: List[Tree]): Option[List[T]]
    }

    def UnliftListOfListsElementwise[T](unliftable: Unliftable[T]): UnliftListOfListsElementwise[T]
    trait UnliftListOfListsElementwise[T] {
      def unapply(lst: List[List[Tree]]): Option[List[List[T]]]
    }

    val SyntacticMatch: SyntacticMatchExtractor
    trait SyntacticMatchExtractor {
      def apply(selector: Tree, cases: List[Tree]): Match
      def unapply(tree: Match): Option[(Tree, List[CaseDef])]
    }

    val SyntacticTry: SyntacticTryExtractor
    trait SyntacticTryExtractor {
      def apply(block: Tree, catches: List[Tree], finalizer: Tree): Try
      def unapply(tree: Try): Option[(Tree, List[CaseDef], Tree)]
    }

    val SyntacticIdent: SyntacticIdentExtractor
    trait SyntacticIdentExtractor {
      def apply(name: Name, isBackquoted: Boolean = false): Ident
      def unapply(tree: Ident): Option[(Name, Boolean)]
    }

    val SyntacticImport: SyntacticImportExtractor
    trait SyntacticImportExtractor {
      def apply(expr: Tree, selectors: List[Tree]): Import
      def unapply(imp: Import): Some[(Tree, List[Tree])]
    }
  }

  @deprecated("Use `internal.reificationSupport` instead", "2.11.0")
  val build: ReificationSupportApi

  /** This trait provides support for importers, a facility to migrate reflection artifacts between universes.
   * ''Note: this trait should typically be used only rarely.''
   *
   *  Reflection artifacts, such as [[scala.reflect.api.Symbols Symbols]] and [[scala.reflect.api.Types Types]],
   *  are contained in [[scala.reflect.api.Universe Universe]]s. Typically all processing happens
   *  within a single `Universe` (e.g. a compile-time macro `Universe` or a runtime reflection `Universe`), but sometimes
   *  there is a need to migrate artifacts from one `Universe` to another. For example, runtime compilation works by
   *  importing runtime reflection trees into a runtime compiler universe, compiling the importees and exporting the
   *  result back.
   *
   *  Reflection artifacts are firmly grounded in their `Universe`s, which is reflected by the fact that types of artifacts
   *  from different universes are not compatible. By using `Importer`s, however, they be imported from one universe
   *  into another. For example, to import `foo.bar.Baz` from the source `Universe` to the target `Universe`,
   *  an importer will first check whether the entire owner chain exists in the target `Universe`.
   *  If it does, then nothing else will be done. Otherwise, the importer will recreate the entire owner chain
   *  and will import the corresponding type signatures into the target `Universe`.
   *
   *  Since importers match `Symbol` tables of the source and the target `Universe`s using plain string names,
   *  it is programmer's responsibility to make sure that imports don't distort semantics, e.g., that
   *  `foo.bar.Baz` in the source `Universe` means the same that `foo.bar.Baz` does in the target `Universe`.
   *
   *  === Example ===
   *
   *  Here's how one might implement a macro that performs compile-time evaluation of its argument
   *  by using a runtime compiler to compile and evaluate a tree that belongs to a compile-time compiler:
   *
   *  {{{
   *  def staticEval[T](x: T) = macro staticEval[T]
   *
   *  def staticEval[T](c: scala.reflect.macros.blackbox.Context)(x: c.Expr[T]) = {
   *    // creates a runtime reflection universe to host runtime compilation
   *    import scala.reflect.runtime.{universe => ru}
   *    val mirror = ru.runtimeMirror(c.libraryClassLoader)
   *    import scala.tools.reflect.ToolBox
   *    val toolBox = mirror.mkToolBox()
   *
   *    // runtime reflection universe and compile-time macro universe are different
   *    // therefore an importer is needed to bridge them
   *    // currently mkImporter requires a cast to correctly assign the path-dependent types
   *    val importer0 = ru.internal.mkImporter(c.universe)
   *    val importer = importer0.asInstanceOf[ru.internal.Importer { val from: c.universe.type }]
   *
   *    // the created importer is used to turn a compiler tree into a runtime compiler tree
   *    // both compilers use the same classpath, so semantics remains intact
   *    val imported = importer.importTree(tree)
   *
   *    // after the tree is imported, it can be evaluated as usual
   *    val tree = toolBox.untypecheck(imported.duplicate)
   *    val valueOfX = toolBox.eval(imported).asInstanceOf[T]
   *    ...
   *  }
   *  }}}
   *
   *  @group Internal
   */
  // SI-6241: move importers to a mirror
  trait Importer {
    /** The source universe of reflection artifacts that will be processed.
     *  The target universe is universe that created this importer with `mkImporter`.
     */
    val from: Universe

    /** An importer that works in reverse direction, namely:
     *  imports reflection artifacts from the current universe to the universe specified in `from`.
     */
    val reverse: from.Importer { val from: self.type }

    /** In the current universe, locates or creates a symbol that corresponds to the provided symbol in the source universe.
     *  If necessary imports the owner chain, companions, type signature, annotations and attachments.
     */
    def importSymbol(sym: from.Symbol): Symbol

    /** In the current universe, locates or creates a type that corresponds to the provided type in the source universe.
     *  If necessary imports the underlying symbols, annotations, scopes and trees.
     */
    def importType(tpe: from.Type): Type

    /** In the current universe, creates a tree that corresponds to the provided tree in the source universe.
     *  If necessary imports the underlying symbols, types and attachments.
     */
    def importTree(tree: from.Tree): Tree

    /** In the current universe, creates a position that corresponds to the provided position in the source universe.
     */
    def importPosition(pos: from.Position): Position
  }

  @deprecated("Use `internal.createImporter` instead", "2.11.0")
  def mkImporter(from0: Universe): Importer { val from: from0.type } = internal.createImporter(from0)

  /** Marks underlying reference to id as boxed.
   *
   *  <b>Precondition:<\b> id must refer to a captured variable
   *  A reference such marked will refer to the boxed entity, no dereferencing
   *  with `.elem` is done on it.
   *  This tree node can be emitted by macros such as reify that call referenceCapturedVariable.
   *  It is eliminated in LambdaLift, where the boxing conversion takes place.
   *  @group Internal
   *  @template
   */
  type ReferenceToBoxed >: Null <: ReferenceToBoxedApi with TermTree

  /** The constructor/extractor for `ReferenceToBoxed` instances.
   *  @group Internal
   */
  val ReferenceToBoxed: ReferenceToBoxedExtractor

  /** An extractor class to create and pattern match with syntax `ReferenceToBoxed(ident)`.
   *  This AST node does not have direct correspondence to Scala code,
   *  and is emitted by macros to reference capture vars directly without going through `elem`.
   *
   *  For example:
   *
   *    var x = ...
   *    fun { x }
   *
   *  Will emit:
   *
   *    Ident(x)
   *
   *  Which gets transformed to:
   *
   *    Select(Ident(x), "elem")
   *
   *  If `ReferenceToBoxed` were used instead of Ident, no transformation would be performed.
   *  @group Internal
   */
  abstract class ReferenceToBoxedExtractor {
    def apply(ident: Ident): ReferenceToBoxed
    def unapply(referenceToBoxed: ReferenceToBoxed): Option[Ident]
  }

  /** The API that all references support
   *  @group Internal
   */
  trait ReferenceToBoxedApi extends TermTreeApi { this: ReferenceToBoxed =>
    /** The underlying reference. */
    def ident: Tree
  }

  /** Tag that preserves the identity of `ReferenceToBoxed` in the face of erasure.
   *  Can be used for pattern matching, instance tests, serialization and the like.
   *  @group Internal
   */
  implicit val ReferenceToBoxedTag: ClassTag[ReferenceToBoxed]

  /** The type of free terms introduced by reification.
   *  @group Internal
   *  @template
   */
  type FreeTermSymbol >: Null <: FreeTermSymbolApi with TermSymbol

  /** The API of free term symbols.
   *  The main source of information about symbols is the [[Symbols]] page.
   *
   *  $SYMACCESSORS
   *  @group Internal
   */
  trait FreeTermSymbolApi extends TermSymbolApi { this: FreeTermSymbol =>
    /** The place where this symbol has been spawned
     *
     *  @group FreeTerm
     */
    def origin: String

    /** The valus this symbol refers to
     *
     *  @group FreeTerm
     */
    def value: Any
  }

  /** Tag that preserves the identity of `FreeTermSymbol` in the face of erasure.
   *  Can be used for pattern matching, instance tests, serialization and the like.
   *  @group Internal
   */
  implicit val FreeTermSymbolTag: ClassTag[FreeTermSymbol]

  /** The type of free types introduced by reification.
   *  @group Internal
   *  @template
   */
  type FreeTypeSymbol >: Null <: FreeTypeSymbolApi with TypeSymbol

  /** The API of free type symbols.
   *  The main source of information about symbols is the [[Symbols]] page.
   *
   *  $SYMACCESSORS
   *  @group Internal
   */
  trait FreeTypeSymbolApi extends TypeSymbolApi { this: FreeTypeSymbol =>
    /** The place where this symbol has been spawned
     *
     *  @group FreeType
     */
    def origin: String
  }

  /** Tag that preserves the identity of `FreeTermSymbol` in the face of erasure.
   *  Can be used for pattern matching, instance tests, serialization and the like.
   *  @group Internal
   */
  implicit val FreeTypeSymbolTag: ClassTag[FreeTypeSymbol]

  /** Provides enrichments to ensure source compatibility between Scala 2.10 and Scala 2.11.
   *  If in your reflective program for Scala 2.10 you've used something that's now become an internal API,
   *  a single `compat._` import will fix things for you.
   *  @group Internal
   */
  val compat: Compat

  /** @see [[compat]]
   *  @group Internal
   */
  type Compat <: CompatApi

  /** @see [[compat]]
   *  @group Internal
   */
  trait CompatApi {
    /** @see [[InternalApi.typeTagToManifest]] */
    @deprecated("Use `internal.typeTagToManifest` instead", "2.11.0")
    def typeTagToManifest[T: ClassTag](mirror: Any, tag: Universe#TypeTag[T]): Manifest[T] =
      internal.typeTagToManifest(mirror, tag)

    /** @see [[InternalApi.manifestToTypeTag]] */
    @deprecated("Use `internal.manifestToTypeTag` instead", "2.11.0")
    def manifestToTypeTag[T](mirror: Any, manifest: Manifest[T]): Universe#TypeTag[T] =
      internal.manifestToTypeTag(mirror, manifest)

    /** @see [[InternalApi.newScopeWith]] */
    @deprecated("Use `internal.newScopeWith` instead", "2.11.0")
    def newScopeWith(elems: Symbol*): Scope =
      internal.newScopeWith(elems: _*)

    /** Scala 2.10 compatibility enrichments for Tree. */
    implicit class CompatibleTree(tree: Tree) {
      /** @see [[InternalApi.freeTerms]] */
      @deprecated("Use `internal.freeTerms` instead", "2.11.0")
      def freeTerms: List[FreeTermSymbol] = internal.freeTerms(tree)

      /** @see [[InternalApi.freeTypes]] */
      @deprecated("Use `internal.freeTerms` instead", "2.11.0")
      def freeTypes: List[FreeTypeSymbol] = internal.freeTypes(tree)

      /** @see [[InternalApi.substituteSymbols]] */
      @deprecated("Use `internal.substituteSymbols` instead", "2.11.0")
      def substituteSymbols(from: List[Symbol], to: List[Symbol]): Tree = internal.substituteSymbols(tree, from, to)

      /** @see [[InternalApi.substituteTypes]] */
      @deprecated("Use `internal.substituteTypes` instead", "2.11.0")
      def substituteTypes(from: List[Symbol], to: List[Type]): Tree = internal.substituteTypes(tree, from, to)

      /** @see [[InternalApi.substituteThis]] */
      @deprecated("Use `internal.substituteThis` instead", "2.11.0")
      def substituteThis(clazz: Symbol, to: Tree): Tree = internal.substituteThis(tree, clazz, to)
    }

    /** Scala 2.10 compatibility enrichments for ClassDef. */
    class CompatibleClassDefExtractor(dex: ClassDefExtractor) {
      /** @see [[InternalApi.classDef]] */
      @deprecated("Use `internal.classDef` instead", "2.11.0")
      def apply(sym: Symbol, impl: Template): ClassDef = internal.classDef(sym, impl)
    }

    /** Scala 2.10 compatibility enrichments for ModuleDef. */
    class CompatibleModuleDefExtractor(dex: ModuleDefExtractor) {
      /** @see [[InternalApi.moduleDef]] */
      @deprecated("Use `internal.moduleDef` instead", "2.11.0")
      def apply(sym: Symbol, impl: Template): ModuleDef = internal.moduleDef(sym, impl)
    }

    /** Scala 2.10 compatibility enrichments for ValDef. */
    class CompatibleValDefExtractor(dex: ValDefExtractor) {
      /** @see [[InternalApi.valDef]] */
      @deprecated("Use `internal.valDef` instead", "2.11.0")
      def apply(sym: Symbol, rhs: Tree): ValDef = internal.valDef(sym, rhs)

      /** @see [[InternalApi.valDef]] */
      @deprecated("Use `internal.valDef` instead", "2.11.0")
      def apply(sym: Symbol): ValDef = internal.valDef(sym)
    }

    /** Scala 2.10 compatibility enrichments for ValDef. */
    class CompatibleDefDefExtractor(dex: DefDefExtractor) {
      /** @see [[InternalApi.defDef]] */
      @deprecated("Use `internal.defDef` instead", "2.11.0")
      def apply(sym: Symbol, mods: Modifiers, vparamss: List[List[ValDef]], rhs: Tree): DefDef = internal.defDef(sym, mods, vparamss, rhs)

      /** @see [[InternalApi.defDef]] */
      @deprecated("Use `internal.defDef` instead", "2.11.0")
      def apply(sym: Symbol, vparamss: List[List[ValDef]], rhs: Tree): DefDef = internal.defDef(sym, vparamss, rhs)

      /** @see [[InternalApi.defDef]] */
      @deprecated("Use `internal.defDef` instead", "2.11.0")
      def apply(sym: Symbol, mods: Modifiers, rhs: Tree): DefDef = internal.defDef(sym, mods, rhs)

      /** @see [[InternalApi.defDef]] */
      @deprecated("Use `internal.defDef` instead", "2.11.0")
      def apply(sym: Symbol, rhs: Tree): DefDef = internal.defDef(sym, rhs)

      /** @see [[InternalApi.defDef]] */
      @deprecated("Use `internal.defDef` instead", "2.11.0")
      def apply(sym: Symbol, rhs: List[List[Symbol]] => Tree): DefDef = internal.defDef(sym, rhs)
    }

    /** Scala 2.10 compatibility enrichments for TypeDef. */
    class CompatibleTypeDefExtractor(dex: TypeDefExtractor) {
      /** @see [[InternalApi.typeDef]] */
      @deprecated("Use `internal.typeDef` instead", "2.11.0")
      def apply(sym: Symbol, rhs: Tree): TypeDef = internal.typeDef(sym, rhs)

      /** @see [[InternalApi.typeDef]] */
      @deprecated("Use `internal.typeDef` instead", "2.11.0")
      def apply(sym: Symbol): TypeDef = internal.typeDef(sym)
    }

    /** Scala 2.10 compatibility enrichments for LabelDef. */
    class CompatibleLabelDefExtractor(dex: LabelDefExtractor) {
      /** @see [[InternalApi.labelDef]] */
      @deprecated("Use `internal.labelDef` instead", "2.11.0")
      def apply(sym: Symbol, params: List[Symbol], rhs: Tree): LabelDef = internal.labelDef(sym, params, rhs)
    }

    /** Scala 2.10 compatibility enrichments for Tree. */
    implicit class CompatibleSymbol(symbol: Symbol) {
      @deprecated("This API is unreliable. Use `isPrivateThis` or `isProtectedThis` instead", "2.11.0")
      def isLocal: Boolean = symbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].isLocal

      @deprecated("This API is unreliable. Use `allOverriddenSymbols.nonEmpty` instead", "2.11.0")
      def isOverride: Boolean = symbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].isOverride

      /** @see [[InternalApi.isFreeTerm]] */
      @deprecated("Use `internal.isFreeTerm` instead", "2.11.0")
      def isFreeTerm: Boolean = internal.isFreeTerm(symbol)

      /** @see [[InternalApi.asFreeTerm]] */
      @deprecated("Use `internal.asFreeTerm` instead", "2.11.0")
      def asFreeTerm: FreeTermSymbol = internal.asFreeTerm(symbol)

      /** @see [[InternalApi.isFreeType]] */
      @deprecated("Use `internal.isFreeType` instead", "2.11.0")
      def isFreeType: Boolean = internal.isFreeType(symbol)

      /** @see [[InternalApi.asFreeType]] */
      @deprecated("Use `internal.asFreeType` instead", "2.11.0")
      def asFreeType: FreeTypeSymbol = internal.asFreeType(symbol)

      /** @see [[InternalApi.asFreeType]] */
      @deprecated("Use `internal.newTermSymbol` instead", "2.11.0")
      def newTermSymbol(name: TermName, pos: Position = NoPosition, flags: FlagSet = NoFlags): TermSymbol = internal.newTermSymbol(symbol, name, pos, flags)

      /** @see [[InternalApi.asFreeType]] */
      @deprecated("Use `internal.newModuleAndClassSymbol` instead", "2.11.0")
      def newModuleAndClassSymbol(name: Name, pos: Position = NoPosition, flags: FlagSet = NoFlags): (ModuleSymbol, ClassSymbol) = internal.newModuleAndClassSymbol(symbol, name, pos, flags)

      /** @see [[InternalApi.asFreeType]] */
      @deprecated("Use `internal.newMethodSymbol` instead", "2.11.0")
      def newMethodSymbol(name: TermName, pos: Position = NoPosition, flags: FlagSet = NoFlags): MethodSymbol = internal.newMethodSymbol(symbol, name, pos, flags)

      /** @see [[InternalApi.asFreeType]] */
      @deprecated("Use `internal.newTypeSymbol` instead", "2.11.0")
      def newTypeSymbol(name: TypeName, pos: Position = NoPosition, flags: FlagSet = NoFlags): TypeSymbol = internal.newTypeSymbol(symbol, name, pos, flags)

      /** @see [[InternalApi.asFreeType]] */
      @deprecated("Use `internal.newClassSymbol` instead", "2.11.0")
      def newClassSymbol(name: TypeName, pos: Position = NoPosition, flags: FlagSet = NoFlags): ClassSymbol = internal.newClassSymbol(symbol, name, pos, flags)

      /** @see [[InternalApi.asFreeType]] */
      @deprecated("Use `internal.isErroneous` instead", "2.11.0")
      def isErroneous: Boolean = internal.isErroneous(symbol)

      /** @see [[InternalApi.asFreeType]] */
      @deprecated("Use `internal.isSkolem` instead", "2.11.0")
      def isSkolem: Boolean = internal.isSkolem(symbol)

      /** @see [[InternalApi.asFreeType]] */
      @deprecated("Use `internal.deSkolemize` instead", "2.11.0")
      def deSkolemize: Symbol = internal.deSkolemize(symbol)
    }

    /** Scala 2.10 compatibility enrichments for ThisType. */
    implicit class CompatibleThisType(tex: ThisTypeExtractor) {
      /** @see [[InternalApi.thisType]] */
      @deprecated("Use `internal.thisType` instead")
      def apply(sym: Symbol): Type = internal.thisType(sym)
    }

    /** Scala 2.10 compatibility enrichments for SingleType. */
    implicit class CompatibleSingleType(tex: SingleTypeExtractor) {
      /** @see [[InternalApi.singleType]] */
      @deprecated("Use `ClassSymbol.thisPrefix` or `internal.singleType` instead")
      def apply(pre: Type, sym: Symbol): Type = internal.singleType(pre, sym)
    }

    /** Scala 2.10 compatibility enrichments for SuperType. */
    implicit class CompatibleSuperType(tex: SuperTypeExtractor) {
      /** @see [[InternalApi.superType]] */
      @deprecated("Use `ClassSymbol.superPrefix` or `internal.superType` instead")
      def apply(thistpe: Type, supertpe: Type): Type = internal.superType(thistpe, supertpe)
    }

    /** Scala 2.10 compatibility enrichments for ConstantType. */
    implicit class CompatibleConstantType(tex: ConstantTypeExtractor) {
      /** @see [[InternalApi.constantType]] */
      @deprecated("Use `value.tpe` or `internal.constantType` instead")
      def apply(value: Constant): ConstantType = internal.constantType(value)
    }

    /** Scala 2.10 compatibility enrichments for TypeRef. */
    implicit class CompatibleTypeRef(tex: TypeRefExtractor) {
      /** @see [[InternalApi.typeRef]] */
      @deprecated("Use `internal.typeRef` instead")
      def apply(pre: Type, sym: Symbol, args: List[Type]): Type = internal.typeRef(pre, sym, args)
    }

    /** Scala 2.10 compatibility enrichments for RefinedType. */
    implicit class CompatibleRefinedType(tex: RefinedTypeExtractor) {
      /** @see [[InternalApi.refinedType]] */
      @deprecated("Use `internal.refinedType` instead")
      def apply(parents: List[Type], decls: Scope): RefinedType = internal.refinedType(parents, decls)
    }

    /** Scala 2.10 compatibility enrichments for ClassInfoType. */
    implicit class CompatibleClassInfoType(tex: ClassInfoTypeExtractor) {
      /** @see [[InternalApi.classInfoType]] */
      @deprecated("Use `internal.classInfoType` instead")
      def apply(parents: List[Type], decls: Scope, typeSymbol: Symbol): ClassInfoType = internal.classInfoType(parents, decls, typeSymbol)
    }

    /** Scala 2.10 compatibility enrichments for MethodType. */
    implicit class CompatibleMethodType(tex: MethodTypeExtractor) {
      /** @see [[InternalApi.methodType]] */
      @deprecated("Use `internal.methodType` instead")
      def apply(params: List[Symbol], resultType: Type): MethodType = internal.methodType(params, resultType)
    }

    /** Scala 2.10 compatibility enrichments for NullaryMethodType. */
    implicit class CompatibleNullaryMethodType(tex: NullaryMethodTypeExtractor) {
      /** @see [[InternalApi.nullaryMethodType]] */
      @deprecated("Use `internal.nullaryMethodType` instead")
      def apply(resultType: Type): NullaryMethodType = internal.nullaryMethodType(resultType)
    }

    /** Scala 2.10 compatibility enrichments for PolyType. */
    implicit class CompatiblePolyType(tex: PolyTypeExtractor) {
      /** @see [[InternalApi.polyType]] */
      @deprecated("Use `internal.polyType` instead")
      def apply(typeParams: List[Symbol], resultType: Type): PolyType = internal.polyType(typeParams, resultType)
    }

    /** Scala 2.10 compatibility enrichments for ExistentialType. */
    implicit class CompatibleExistentialType(tex: ExistentialTypeExtractor) {
      /** @see [[InternalApi.existentialType]] */
      @deprecated("Use `internal.existentialType` instead")
      def apply(quantified: List[Symbol], underlying: Type): ExistentialType = internal.existentialType(quantified, underlying)
    }

    /** Scala 2.10 compatibility enrichments for AnnotatedType. */
    implicit class CompatibleAnnotatedType(tex: AnnotatedTypeExtractor) {
      /** @see [[InternalApi.annotatedType]] */
      @deprecated("Use `internal.annotatedType` instead")
      def apply(annotations: List[Annotation], underlying: Type): AnnotatedType = internal.annotatedType(annotations, underlying)
    }

    /** Scala 2.10 compatibility enrichments for TypeBounds. */
    implicit class CompatibleTypeBounds(tex: TypeBoundsExtractor) {
      /** @see [[InternalApi.typeBounds]] */
      @deprecated("Use `internal.typeBounds` instead")
      def apply(lo: Type, hi: Type): TypeBounds = internal.typeBounds(lo, hi)
    }

    /** Scala 2.10 compatibility enrichments for BoundedWildcardType. */
    implicit class CompatibleBoundedWildcardType(tex: BoundedWildcardTypeExtractor) {
      /** @see [[InternalApi.boundedWildcardType]] */
      @deprecated("Use `internal.boundedWildcardType` instead")
      def apply(bounds: TypeBounds): BoundedWildcardType = internal.boundedWildcardType(bounds)
    }
  }
}
