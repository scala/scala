package scala
package reflect
package api

import scala.language.implicitConversions
import scala.language.higherKinds

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
  trait InternalApi { internal =>
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
     *  `C.this`, where `C` refers to the given class `clazz`.
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

    /** Returns internal flags associated with the symbol.
     */
    def flags(symbol: Symbol): FlagSet

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
    def refinedType(parents: List[Type], decls: Scope, clazz: Symbol): RefinedType

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

    /** Syntactic conveniences for additional internal APIs for trees, symbols and types */
    type Decorators <: DecoratorApi

    /** @see [[Decorators]] */
    val decorators: Decorators

    /** @see [[Decorators]] */
    trait DecoratorApi {
      /** Extension methods for trees */
      type TreeDecorator[T <: Tree] <: TreeDecoratorApi[T]

      /** @see [[TreeDecorator]] */
      implicit def treeDecorator[T <: Tree](tree: T): TreeDecorator[T]

      /** @see [[TreeDecorator]] */
      class TreeDecoratorApi[T <: Tree](val tree: T) {
        /** @see [[internal.freeTerms]] */
        def freeTerms: List[FreeTermSymbol] = internal.freeTerms(tree)

        /** @see [[internal.freeTypes]] */
        def freeTypes: List[FreeTypeSymbol] = internal.freeTypes(tree)

        /** @see [[internal.substituteSymbols]] */
        def substituteSymbols(from: List[Symbol], to: List[Symbol]): Tree = internal.substituteSymbols(tree, from, to)

        /** @see [[internal.substituteTypes]] */
        def substituteTypes(from: List[Symbol], to: List[Type]): Tree = internal.substituteTypes(tree, from, to)

        /** @see [[internal.substituteThis]] */
        def substituteThis(clazz: Symbol, to: Tree): Tree = internal.substituteThis(tree, clazz, to)
      }

      /** Extension methods for symbols */
      type SymbolDecorator[T <: Symbol] <: SymbolDecoratorApi[T]

      /** @see [[SymbolDecorator]] */
      implicit def symbolDecorator[T <: Symbol](symbol: T): SymbolDecorator[T]

      /** @see [[SymbolDecorator]] */
      class SymbolDecoratorApi[T <: Symbol](val symbol: T) {
        /** @see [[internal.isFreeTerm]] */
        def isFreeTerm: Boolean = internal.isFreeTerm(symbol)

        /** @see [[internal.asFreeTerm]] */
        def asFreeTerm: FreeTermSymbol = internal.asFreeTerm(symbol)

        /** @see [[internal.isFreeType]] */
        def isFreeType: Boolean = internal.isFreeType(symbol)

        /** @see [[internal.asFreeType]] */
        def asFreeType: FreeTypeSymbol = internal.asFreeType(symbol)

        /** @see [[internal.newTermSymbol]] */
        def newTermSymbol(name: TermName, pos: Position = NoPosition, flags: FlagSet = NoFlags): TermSymbol = internal.newTermSymbol(symbol, name, pos, flags)

        /** @see [[internal.newModuleAndClassSymbol]] */
        def newModuleAndClassSymbol(name: Name, pos: Position = NoPosition, flags: FlagSet = NoFlags): (ModuleSymbol, ClassSymbol) = internal.newModuleAndClassSymbol(symbol, name, pos, flags)

        /** @see [[internal.newMethodSymbol]] */
        def newMethodSymbol(name: TermName, pos: Position = NoPosition, flags: FlagSet = NoFlags): MethodSymbol = internal.newMethodSymbol(symbol, name, pos, flags)

        /** @see [[internal.newTypeSymbol]] */
        def newTypeSymbol(name: TypeName, pos: Position = NoPosition, flags: FlagSet = NoFlags): TypeSymbol = internal.newTypeSymbol(symbol, name, pos, flags)

        /** @see [[internal.newClassSymbol]] */
        def newClassSymbol(name: TypeName, pos: Position = NoPosition, flags: FlagSet = NoFlags): ClassSymbol = internal.newClassSymbol(symbol, name, pos, flags)

        /** @see [[internal.isErroneous]] */
        def isErroneous: Boolean = internal.isErroneous(symbol)

        /** @see [[internal.isSkolem]] */
        def isSkolem: Boolean = internal.isSkolem(symbol)

        /** @see [[internal.deSkolemize]] */
        def deSkolemize: Symbol = internal.deSkolemize(symbol)

        /** @see [[internal.initialize]] */
        def initialize: T = internal.initialize(symbol)

        /** @see [[internal.fullyInitialize]] */
        def fullyInitialize: T = internal.fullyInitialize(symbol)

        /** @see [[internal.flags]] */
        def flags: FlagSet = internal.flags(symbol)
      }

      /** Extension methods for types */
      type TypeDecorator[T <: Type] <: TypeDecoratorApi[T]

      /** @see [[TypeDecorator]] */
      implicit def typeDecorator[T <: Type](tp: T): TypeDecorator[T]

      /** @see [[TypeDecorator]] */
      implicit class TypeDecoratorApi[T <: Type](val tp: T) {
        /** @see [[internal.fullyInitialize]] */
        def fullyInitialize: T = internal.fullyInitialize(tp)
      }
    }
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
    def setInfo[S <: Symbol](sym: S, tpe: Type): S

    /** Set symbol's annotations to given annotations `annots`.
     */
    def setAnnotations[S <: Symbol](sym: S, annots: List[Annotation]): S

    def mkThis(sym: Symbol): Tree

    def mkSelect(qualifier: Tree, sym: Symbol): Select

    def mkIdent(sym: Symbol): Ident

    def mkTypeTree(tp: Type): TypeTree

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

    def mkRefTree(qual: Tree, sym: Symbol): Tree

    def freshTermName(prefix: String): TermName

    def freshTypeName(prefix: String): TypeName

    val ImplicitParams: ImplicitParamsExtractor

    trait ImplicitParamsExtractor {
      def apply(paramss: List[List[Tree]], implparams: List[Tree]): List[List[Tree]]
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
    val SyntacticAppliedType: SyntacticTypeAppliedExtractor

    trait SyntacticTypeAppliedExtractor {
      def apply(tree: Tree, targs: List[Tree]): Tree
      def unapply(tree: Tree): Option[(Tree, List[Tree])]
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

    val SyntacticPatDef: SyntacticPatDefExtractor

    trait SyntacticPatDefExtractor {
      def apply(mods: Modifiers, pat: Tree, tpt: Tree, rhs: Tree): List[ValDef]
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

    val SyntacticPartialFunction: SyntacticPartialFunctionExtractor
    trait SyntacticPartialFunctionExtractor {
      def apply(cases: List[Tree]): Match
      def unapply(tree: Tree): Option[List[CaseDef]]
    }

    val SyntacticMatch: SyntacticMatchExtractor
    trait SyntacticMatchExtractor {
      def apply(scrutinee: Tree, cases: List[Tree]): Match
      def unapply(tree: Match): Option[(Tree, List[CaseDef])]
    }

    val SyntacticTry: SyntacticTryExtractor
    trait SyntacticTryExtractor {
      def apply(block: Tree, catches: List[Tree], finalizer: Tree): Try
      def unapply(tree: Try): Option[(Tree, List[CaseDef], Tree)]
    }

    val SyntacticTermIdent: SyntacticTermIdentExtractor
    trait SyntacticTermIdentExtractor {
      def apply(name: TermName, isBackquoted: Boolean = false): Ident
      def unapply(id: Ident): Option[(TermName, Boolean)]
    }

    val SyntacticTypeIdent: SyntacticTypeIdentExtractor
    trait SyntacticTypeIdentExtractor {
      def apply(name: TypeName): Ident
      def unapply(tree: Tree): Option[TypeName]
    }

    val SyntacticImport: SyntacticImportExtractor
    trait SyntacticImportExtractor {
      def apply(expr: Tree, selectors: List[Tree]): Import
      def unapply(imp: Import): Some[(Tree, List[Tree])]
    }

    val SyntacticSelectType: SyntacticSelectTypeExtractor
    trait SyntacticSelectTypeExtractor {
      def apply(qual: Tree, name: TypeName): Select
      def unapply(tree: Tree): Option[(Tree, TypeName)]
    }

    val SyntacticSelectTerm: SyntacticSelectTermExtractor
    trait SyntacticSelectTermExtractor {
      def apply(qual: Tree, name: TermName): Select
      def unapply(tree: Tree): Option[(Tree, TermName)]
    }

    val SyntacticCompoundType: SyntacticCompoundTypeExtractor
    trait SyntacticCompoundTypeExtractor {
      def apply(parents: List[Tree], defns: List[Tree]): CompoundTypeTree
      def unapply(tree: Tree): Option[(List[Tree], List[Tree])]
    }

    val SyntacticSingletonType: SyntacitcSingletonTypeExtractor
    trait SyntacitcSingletonTypeExtractor {
      def apply(tree: Tree): SingletonTypeTree
      def unapply(tree: Tree): Option[Tree]
    }

    val SyntacticTypeProjection: SyntacticTypeProjectionExtractor
    trait SyntacticTypeProjectionExtractor {
      def apply(qual: Tree, name: TypeName): SelectFromTypeTree
      def unapply(tree: Tree): Option[(Tree, TypeName)]
    }

    val SyntacticAnnotatedType: SyntacticAnnotatedTypeExtractor
    trait SyntacticAnnotatedTypeExtractor {
      def apply(tpt: Tree, annot: Tree): Annotated
      def unapply(tree: Tree): Option[(Tree, Tree)]
    }

    val SyntacticExistentialType: SyntacticExistentialTypeExtractor
    trait SyntacticExistentialTypeExtractor {
      def apply(tpt: Tree, where: List[Tree]): ExistentialTypeTree
      def unapply(tree: Tree): Option[(Tree, List[MemberDef])]
    }
  }

  @deprecated("Use `internal.reificationSupport` instead", "2.11.0")
  val build: ReificationSupportApi

  @deprecated("Use `internal.ReificationSupportApi` instead", "2.11.0")
  type BuildApi = ReificationSupportApi

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
   *  <b>Precondition:</b> id must refer to a captured variable
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

  /** Presence of an implicit value of this type in scope
   *  indicates that source compatibility with Scala 2.10 has been enabled.
   *  @group Internal
   */
  @scala.annotation.implicitNotFound("This method has been removed from the public API. Import compat._ or migrate away.")
  class CompatToken

  /** @see [[compat]]
   *  @group Internal
   */
  trait CompatApi {
    /** @see [[CompatToken]] */
    implicit val token = new CompatToken

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

    /** Scala 2.10 compatibility enrichments for BuildApi. */
    implicit class CompatibleBuildApi(api: BuildApi) {
      /** @see [[BuildApi.setInfo]] */
      @deprecated("Use `internal.reificationSupport.setInfo` instead", "2.11.0")
      def setTypeSignature[S <: Symbol](sym: S, tpe: Type): S = internal.reificationSupport.setInfo(sym, tpe)

      /** @see [[BuildApi.FlagsRepr]] */
      @deprecated("Use `internal.reificationSupport.FlagsRepr` instead", "2.11.0")
      def flagsFromBits(bits: Long): FlagSet = internal.reificationSupport.FlagsRepr(bits)

      /** @see [[BuildApi.noSelfType]] */
      @deprecated("Use `noSelfType` instead", "2.11.0")
      def emptyValDef: ValDef = noSelfType

      /** @see [[BuildApi.mkThis]] */
      @deprecated("Use `internal.reificationSupport.mkThis` instead", "2.11.0")
      def This(sym: Symbol): Tree = internal.reificationSupport.mkThis(sym)

      /** @see [[BuildApi.mkSelect]] */
      @deprecated("Use `internal.reificationSupport.mkSelect` instead", "2.11.0")
      def Select(qualifier: Tree, sym: Symbol): Select = internal.reificationSupport.mkSelect(qualifier, sym)

      /** @see [[BuildApi.mkIdent]] */
      @deprecated("Use `internal.reificationSupport.mkIdent` instead", "2.11.0")
      def Ident(sym: Symbol): Ident = internal.reificationSupport.mkIdent(sym)

      /** @see [[BuildApi.mkTypeTree]] */
      @deprecated("Use `internal.reificationSupport.mkTypeTree` instead", "2.11.0")
      def TypeTree(tp: Type): TypeTree = internal.reificationSupport.mkTypeTree(tp)
    }

    /** Scala 2.10 compatibility enrichments for Tree. */
    implicit class CompatibleTree(tree: Tree) {
      /** @see [[InternalApi.freeTerms]] */
      @deprecated("Use `internal.freeTerms` instead or import `internal.decorators._` for infix syntax", "2.11.0")
      def freeTerms: List[FreeTermSymbol] = internal.freeTerms(tree)

      /** @see [[InternalApi.freeTypes]] */
      @deprecated("Use `internal.freeTerms` instead or import `internal.decorators._` for infix syntax", "2.11.0")
      def freeTypes: List[FreeTypeSymbol] = internal.freeTypes(tree)

      /** @see [[InternalApi.substituteSymbols]] */
      @deprecated("Use `internal.substituteSymbols` instead or import `internal.decorators._` for infix syntax", "2.11.0")
      def substituteSymbols(from: List[Symbol], to: List[Symbol]): Tree = internal.substituteSymbols(tree, from, to)

      /** @see [[InternalApi.substituteTypes]] */
      @deprecated("Use `internal.substituteTypes` instead or import `internal.decorators._` for infix syntax", "2.11.0")
      def substituteTypes(from: List[Symbol], to: List[Type]): Tree = internal.substituteTypes(tree, from, to)

      /** @see [[InternalApi.substituteThis]] */
      @deprecated("Use `internal.substituteThis` instead or import `internal.decorators._` for infix syntax", "2.11.0")
      def substituteThis(clazz: Symbol, to: Tree): Tree = internal.substituteThis(tree, clazz, to)
    }

    /** Scala 2.10 compatibility enrichments for Tree. */
    implicit class CompatibleSymbol(symbol: Symbol) {
      @deprecated("This API is unreliable. Use `isPrivateThis` or `isProtectedThis` instead", "2.11.0")
      def isLocal: Boolean = symbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].isLocal

      @deprecated("This API is unreliable. Use `overrides.nonEmpty` instead", "2.11.0")
      def isOverride: Boolean = symbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].isOverride

      /** @see [[InternalApi.isFreeTerm]] */
      @deprecated("Use `internal.isFreeTerm` instead or import `internal.decorators._` for infix syntax", "2.11.0")
      def isFreeTerm: Boolean = internal.isFreeTerm(symbol)

      /** @see [[InternalApi.asFreeTerm]] */
      @deprecated("Use `internal.asFreeTerm` instead or import `internal.decorators._` for infix syntax", "2.11.0")
      def asFreeTerm: FreeTermSymbol = internal.asFreeTerm(symbol)

      /** @see [[InternalApi.isFreeType]] */
      @deprecated("Use `internal.isFreeType` instead or import `internal.decorators._` for infix syntax", "2.11.0")
      def isFreeType: Boolean = internal.isFreeType(symbol)

      /** @see [[InternalApi.asFreeType]] */
      @deprecated("Use `internal.asFreeType` instead or import `internal.decorators._` for infix syntax", "2.11.0")
      def asFreeType: FreeTypeSymbol = internal.asFreeType(symbol)

      /** @see [[InternalApi.asFreeType]] */
      @deprecated("Use `internal.newTermSymbol` instead or import `internal.decorators._` for infix syntax", "2.11.0")
      def newTermSymbol(name: TermName, pos: Position = NoPosition, flags: FlagSet = NoFlags): TermSymbol = internal.newTermSymbol(symbol, name, pos, flags)

      /** @see [[InternalApi.asFreeType]] */
      @deprecated("Use `internal.newModuleAndClassSymbol` instead or import `internal.decorators._` for infix syntax", "2.11.0")
      def newModuleAndClassSymbol(name: Name, pos: Position = NoPosition, flags: FlagSet = NoFlags): (ModuleSymbol, ClassSymbol) = internal.newModuleAndClassSymbol(symbol, name, pos, flags)

      /** @see [[InternalApi.asFreeType]] */
      @deprecated("Use `internal.newMethodSymbol` instead or import `internal.decorators._` for infix syntax", "2.11.0")
      def newMethodSymbol(name: TermName, pos: Position = NoPosition, flags: FlagSet = NoFlags): MethodSymbol = internal.newMethodSymbol(symbol, name, pos, flags)

      /** @see [[InternalApi.asFreeType]] */
      @deprecated("Use `internal.newTypeSymbol` instead or import `internal.decorators._` for infix syntax", "2.11.0")
      def newTypeSymbol(name: TypeName, pos: Position = NoPosition, flags: FlagSet = NoFlags): TypeSymbol = internal.newTypeSymbol(symbol, name, pos, flags)

      /** @see [[InternalApi.asFreeType]] */
      @deprecated("Use `internal.newClassSymbol` instead or import `internal.decorators._` for infix syntax", "2.11.0")
      def newClassSymbol(name: TypeName, pos: Position = NoPosition, flags: FlagSet = NoFlags): ClassSymbol = internal.newClassSymbol(symbol, name, pos, flags)

      /** @see [[InternalApi.asFreeType]] */
      @deprecated("Use `internal.isErroneous` instead or import `internal.decorators._` for infix syntax", "2.11.0")
      def isErroneous: Boolean = internal.isErroneous(symbol)

      /** @see [[InternalApi.asFreeType]] */
      @deprecated("Use `internal.isSkolem` instead or import `internal.decorators._` for infix syntax", "2.11.0")
      def isSkolem: Boolean = internal.isSkolem(symbol)

      /** @see [[InternalApi.asFreeType]] */
      @deprecated("Use `internal.deSkolemize` instead or import `internal.decorators._` for infix syntax", "2.11.0")
      def deSkolemize: Symbol = internal.deSkolemize(symbol)
    }

    /** @see [[InternalApi.singleType]] */
    @deprecated("Use `internal.singleType` instead", "2.11.0")
    def singleType(pre: Type, sym: Symbol): Type = internal.singleType(pre, sym)

    /** @see [[InternalApi.refinedType]] */
    @deprecated("Use `internal.refinedType` instead", "2.11.0")
    def refinedType(parents: List[Type], owner: Symbol, decls: Scope, pos: Position): Type = internal.refinedType(parents, owner, decls, pos)

    /** @see [[InternalApi.refinedType]] */
    @deprecated("Use `internal.refinedType` instead", "2.11.0")
    def refinedType(parents: List[Type], owner: Symbol): Type = internal.refinedType(parents, owner)

    /** @see [[InternalApi.typeRef]] */
    @deprecated("Use `internal.typeRef` instead", "2.11.0")
    def typeRef(pre: Type, sym: Symbol, args: List[Type]): Type = internal.typeRef(pre, sym, args)

    /** @see [[InternalApi.intersectionType]] */
    @deprecated("Use `internal.intersectionType` instead", "2.11.0")
    def intersectionType(tps: List[Type]): Type = internal.intersectionType(tps)

    /** @see [[InternalApi.intersectionType]] */
    @deprecated("Use `internal.intersectionType` instead", "2.11.0")
    def intersectionType(tps: List[Type], owner: Symbol): Type = internal.intersectionType(tps, owner)

    /** @see [[InternalApi.polyType]] */
    @deprecated("Use `internal.polyType` instead", "2.11.0")
    def polyType(tparams: List[Symbol], tpe: Type): Type = internal.polyType(tparams, tpe)

    /** @see [[InternalApi.existentialAbstraction]] */
    @deprecated("Use `internal.existentialAbstraction` instead", "2.11.0")
    def existentialAbstraction(tparams: List[Symbol], tpe0: Type): Type = internal.existentialAbstraction(tparams, tpe0)
  }
}
