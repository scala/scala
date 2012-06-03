/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */
package scala.reflect
package api

// Syncnote: Trees are currently not thread-safe.
trait Trees extends base.Trees { self: Universe =>

  override type Tree >: Null <: TreeApi

  /** ... */
  trait TreeApi extends TreeBase { this: Tree =>

    /** ... */
    def pos: Position

    /** ... */
    def tpe: Type

    /** Note that symbol is fixed as null at this level.  In SymTrees,
     *  it is overridden and implemented with a var, initialized to NoSymbol.
     *
     *  Trees which are not SymTrees but which carry symbols do so by
     *  overriding `def symbol` to forward it elsewhere.  Examples:
     *
     *    Super(qual, _)              // has qual's symbol
     *    Apply(fun, args)            // has fun's symbol
     *    TypeApply(fun, args)        // has fun's symbol
     *    AppliedTypeTree(tpt, args)  // has tpt's symbol
     *    TypeTree(tpe)               // has tpe's typeSymbol, if tpe != null
     *
     *  Attempting to set the symbol of a Tree which does not support
     *  it will induce an exception.
     */
    def symbol: Symbol

    /** ... */
    def hasSymbol: Boolean

    /** Provides an alternate if tree is empty
     *  @param  alt  The alternate tree
     *  @return If this tree is non empty, this tree, otherwise `alt`.
     */
    def orElse(alt: => Tree): Tree

    /** Apply `f` to each subtree */
    def foreach(f: Tree => Unit): Unit

    /** Find all subtrees matching predicate `p`. Same as `filter` */
    def withFilter(f: Tree => Boolean): List[Tree]

    /** Find all subtrees matching predicate `p`. Same as `withFilter` */
    def filter(f: Tree => Boolean): List[Tree]

    /** Apply `pf' to each subtree on which the function is defined and collect the results.
     */
    def collect[T](pf: PartialFunction[Tree, T]): List[T]

    /** Returns optionally first tree (in a preorder traversal) which satisfies predicate `p`,
     *  or None if none exists.
     */
    def find(p: Tree => Boolean): Option[Tree]

    /** Is there exists a part of this tree which satisfies predicate `p`? */
    def exists(p: Tree => Boolean): Boolean

    /** Do all parts of this tree satisfy predicate `p`? */
    def forAll(p: Tree => Boolean): Boolean

    /** Tests whether two trees are structurall equal.
     *  Note that `==` on trees is reference equality.
     */
    def equalsStructure(that : Tree): Boolean

    /** The direct child trees of this tree.
     *  EmptyTrees are always omitted.  Lists are flattened.
     */
    def children: List[Tree]

    /** Extracts free term symbols from a tree that is reified or contains reified subtrees.
     */
    def freeTerms: List[FreeTermSymbol]

    /** Extracts free type symbols from a tree that is reified or contains reified subtrees.
     */
    def freeTypes: List[FreeTypeSymbol]

    /** Substitute symbols in `to` for corresponding occurrences of references to
     *  symbols `from` in this type.
     */
    def substituteSymbols(from: List[Symbol], to: List[Symbol]): Tree

    /** Substitute types in `to` for corresponding occurrences of references to
     *  symbols `from` in this tree.
     */
    def substituteTypes(from: List[Symbol], to: List[Type]): Tree

    /** Substitute given tree `to` for occurrences of nodes that represent
     *  `C.this`, where `C` referes to the given class `clazz`.
     */
    def substituteThis(clazz: Symbol, to: Tree): Tree

    /** Make a copy of this tree, keeping all attributes,
     *  except that all positions are focused (so nothing
     *  in this tree will be found when searching by position).
     */
    def duplicate: this.type
  }

  override type TermTree >: Null <: Tree with TermTreeApi

  /** The API that all term trees support */
  trait TermTreeApi extends TreeApi { this: TermTree =>
  }

  override type TypTree >: Null <: Tree with TypTreeApi

  /** The API that all typ trees support */
  trait TypTreeApi extends TreeApi { this: TypTree =>
  }

  override type SymTree >: Null <: Tree with SymTreeApi

  /** The API that all sym trees support */
  trait SymTreeApi extends TreeApi { this: SymTree =>
    def symbol: Symbol
  }

  override type NameTree >: Null <: Tree with NameTreeApi

  /** The API that all name trees support */
  trait NameTreeApi extends TreeApi { this: NameTree =>
    def name: Name
  }

  override type RefTree >: Null <: SymTree with NameTree with RefTreeApi

  /** The API that all ref trees support */
  trait RefTreeApi extends SymTreeApi with NameTreeApi { this: RefTree =>
    def qualifier: Tree    // empty for Idents
    def name: Name
  }

  override type DefTree >: Null <: SymTree with NameTree with DefTreeApi

  /** The API that all def trees support */
  trait DefTreeApi extends SymTreeApi with NameTreeApi { this: DefTree =>
    def name: Name
  }

  override type MemberDef >: Null <: DefTree with MemberDefApi

  /** The API that all member defs support */
  trait MemberDefApi extends DefTreeApi { this: MemberDef =>
    def mods: Modifiers
  }

  override type PackageDef >: Null <: MemberDef with PackageDefApi

  /** The API that all package defs support */
  trait PackageDefApi extends MemberDefApi { this: PackageDef =>
    val pid: RefTree
    val stats: List[Tree]
  }

  override type ImplDef >: Null <: MemberDef with ImplDefApi

  /** The API that all impl defs support */
  trait ImplDefApi extends MemberDefApi { this: ImplDef =>
    val impl: Template
  }

  override type ClassDef >: Null <: ImplDef with ClassDefApi

  /** The API that all class defs support */
  trait ClassDefApi extends ImplDefApi { this: ClassDef =>
    val mods: Modifiers
    val name: TypeName
    val tparams: List[TypeDef]
    val impl: Template
  }

  override type ModuleDef >: Null <: ImplDef with ModuleDefApi

  /** The API that all module defs support */
  trait ModuleDefApi extends ImplDefApi { this: ModuleDef =>
    val mods: Modifiers
    val name: TermName
    val impl: Template
  }

  override type ValOrDefDef >: Null <: MemberDef with ValOrDefDefApi

  /** The API that all val defs and def defs support */
  trait ValOrDefDefApi extends MemberDefApi { this: ValOrDefDef =>
    def name: Name // can't be a TermName because macros can be type names.
    def tpt: Tree
    def rhs: Tree
  }

  override type ValDef >: Null <: ValOrDefDef with ValDefApi

  /** The API that all val defs support */
  trait ValDefApi extends ValOrDefDefApi { this: ValDef =>
    val mods: Modifiers
    val name: TermName
    val tpt: Tree
    val rhs: Tree
  }

  override type DefDef >: Null <: ValOrDefDef with DefDefApi

  /** The API that all def defs support */
  trait DefDefApi extends ValOrDefDefApi { this: DefDef =>
    val mods: Modifiers
    val name: Name
    val tparams: List[TypeDef]
    val vparamss: List[List[ValDef]]
    val tpt: Tree
    val rhs: Tree
  }

  override type TypeDef >: Null <: MemberDef with TypeDefApi

  /** The API that all type defs support */
  trait TypeDefApi extends MemberDefApi { this: TypeDef =>
    val mods: Modifiers
    val name: TypeName
    val tparams: List[TypeDef]
    val rhs: Tree
  }

  override type LabelDef >: Null <: DefTree with TermTree with LabelDefApi

  /** The API that all label defs support */
  trait LabelDefApi extends DefTreeApi with TermTreeApi { this: LabelDef =>
    val name: TermName
    val params: List[Ident]
    val rhs: Tree
  }

  override type ImportSelector >: Null <: ImportSelectorApi

  /** The API that all import selectors support */
  trait ImportSelectorApi { this: ImportSelector =>
    val name: Name
    val namePos: Int
    val rename: Name
    val renamePos: Int
  }

  override type Import >: Null <: SymTree with ImportApi

  /** The API that all imports support */
  trait ImportApi extends SymTreeApi { this: Import =>
    val expr: Tree
    val selectors: List[ImportSelector]
  }

  override type Template >: Null <: SymTree with TemplateApi

  /** The API that all templates support */
  trait TemplateApi extends SymTreeApi { this: Template =>
    val parents: List[Tree]
    val self: ValDef
    val body: List[Tree]
  }

  override type Block >: Null <: TermTree with BlockApi

  /** The API that all blocks support */
  trait BlockApi extends TermTreeApi { this: Block =>
    val stats: List[Tree]
    val expr: Tree
  }

  override type CaseDef >: Null <: Tree with CaseDefApi

  /** The API that all case defs support */
  trait CaseDefApi extends TreeApi { this: CaseDef =>
    val pat: Tree
    val guard: Tree
    val body: Tree
  }

  override type Alternative >: Null <: TermTree with AlternativeApi

  /** The API that all alternatives support */
  trait AlternativeApi extends TermTreeApi { this: Alternative =>
    val trees: List[Tree]
  }

  override type Star >: Null <: TermTree with StarApi

  /** The API that all stars support */
  trait StarApi extends TermTreeApi { this: Star =>
    val elem: Tree
  }

  override type Bind >: Null <: DefTree with BindApi

  /** The API that all binds support */
  trait BindApi extends DefTreeApi { this: Bind =>
    val name: Name
    val body: Tree
  }

  override type UnApply >: Null <: TermTree with UnApplyApi

  /** The API that all unapplies support */
  trait UnApplyApi extends TermTreeApi { this: UnApply =>
    val fun: Tree
    val args: List[Tree]
  }

  override type ArrayValue >: Null <: TermTree with ArrayValueApi

  /** The API that all array values support */
  trait ArrayValueApi extends TermTreeApi { this: ArrayValue =>
    val elemtpt: Tree
    val elems: List[Tree]
  }

  override type Function >: Null <: TermTree with SymTree with FunctionApi

  /** The API that all functions support */
  trait FunctionApi extends TermTreeApi with SymTreeApi { this: Function =>
    val vparams: List[ValDef]
    val body: Tree
  }

  override type Assign >: Null <: TermTree with AssignApi

  /** The API that all assigns support */
  trait AssignApi extends TermTreeApi { this: Assign =>
    val lhs: Tree
    val rhs: Tree
  }

  override type AssignOrNamedArg >: Null <: TermTree with AssignOrNamedArgApi

  /** The API that all assigns support */
  trait AssignOrNamedArgApi extends TermTreeApi { this: AssignOrNamedArg =>
    val lhs: Tree
    val rhs: Tree
  }

  override type If >: Null <: TermTree with IfApi

  /** The API that all ifs support */
  trait IfApi extends TermTreeApi { this: If =>
    val cond: Tree
    val thenp: Tree
    val elsep: Tree
  }

  override type Match >: Null <: TermTree with MatchApi

  /** The API that all matches support */
  trait MatchApi extends TermTreeApi { this: Match =>
    val selector: Tree
    val cases: List[CaseDef]
  }

  override type Return >: Null <: TermTree with SymTree with ReturnApi

  /** The API that all returns support */
  trait ReturnApi extends TermTreeApi { this: Return =>
    val expr: Tree
  }

  override type Try >: Null <: TermTree with TryApi

  /** The API that all tries support */
  trait TryApi extends TermTreeApi { this: Try =>
    val block: Tree
    val catches: List[CaseDef]
    val finalizer: Tree
  }

  override type Throw >: Null <: TermTree with ThrowApi

  /** The API that all tries support */
  trait ThrowApi extends TermTreeApi { this: Throw =>
    val expr: Tree
  }

  override type New >: Null <: TermTree with NewApi

  /** The API that all news support */
  trait NewApi extends TermTreeApi { this: New =>
    val tpt: Tree
  }

  override type Typed >: Null <: TermTree with TypedApi

  /** The API that all typeds support */
  trait TypedApi extends TermTreeApi { this: Typed =>
    val expr: Tree
    val tpt: Tree
  }

  override type GenericApply >: Null <: TermTree with GenericApplyApi

  /** The API that all applies support */
  trait GenericApplyApi extends TermTreeApi { this: GenericApply =>
    val fun: Tree
    val args: List[Tree]
  }

  override type TypeApply >: Null <: GenericApply with TypeApplyApi

  /** The API that all type applies support */
  trait TypeApplyApi extends GenericApplyApi { this: TypeApply =>
  }

  override type Apply >: Null <: GenericApply with ApplyApi

  /** The API that all applies support */
  trait ApplyApi extends GenericApplyApi { this: Apply =>
  }

  override type ApplyDynamic >: Null <: TermTree with SymTree with ApplyDynamicApi

  /** The API that all apply dynamics support */
  trait ApplyDynamicApi extends TermTreeApi with SymTreeApi { this: ApplyDynamic =>
    val qual: Tree
    val args: List[Tree]
  }

  override type Super >: Null <: TermTree with SuperApi

  /** The API that all supers support */
  trait SuperApi extends TermTreeApi { this: Super =>
    val qual: Tree
    val mix: TypeName
  }

  override type This >: Null <: TermTree with SymTree with ThisApi

  /** The API that all thises support */
  trait ThisApi extends TermTreeApi with SymTreeApi { this: This =>
    val qual: TypeName
  }

  override type Select >: Null <: RefTree with SelectApi

  /** The API that all selects support */
  trait SelectApi extends RefTreeApi { this: Select =>
    val qualifier: Tree
    val name: Name
  }

  override type Ident >: Null <: RefTree with IdentApi

  /** The API that all idents support */
  trait IdentApi extends RefTreeApi { this: Ident =>
    val name: Name
  }

  override type ReferenceToBoxed >: Null <: TermTree with ReferenceToBoxedApi

  /** The API that all references support */
  trait ReferenceToBoxedApi extends TermTreeApi { this: ReferenceToBoxed =>
    val ident: Tree
  }

  override type Literal >: Null <: TermTree with LiteralApi

  /** The API that all literals support */
  trait LiteralApi extends TermTreeApi { this: Literal =>
    val value: Constant
  }

  override type Annotated >: Null <: Tree with AnnotatedApi

  /** The API that all annotateds support */
  trait AnnotatedApi extends TreeApi { this: Annotated =>
    val annot: Tree
    val arg: Tree
  }

  override type SingletonTypeTree >: Null <: TypTree with SingletonTypeTreeApi

  /** The API that all singleton type trees support */
  trait SingletonTypeTreeApi extends TypTreeApi { this: SingletonTypeTree =>
    val ref: Tree
  }

  override type SelectFromTypeTree >: Null <: TypTree with RefTree with SelectFromTypeTreeApi

  /** The API that all selects from type trees support */
  trait SelectFromTypeTreeApi extends TypTreeApi with RefTreeApi { this: SelectFromTypeTree =>
    val qualifier: Tree
    val name: TypeName
  }

  override type CompoundTypeTree >: Null <: TypTree with CompoundTypeTreeApi

  /** The API that all compound type trees support */
  trait CompoundTypeTreeApi extends TypTreeApi { this: CompoundTypeTree =>
    val templ: Template
  }

  override type AppliedTypeTree >: Null <: TypTree with AppliedTypeTreeApi

  /** The API that all applied type trees support */
  trait AppliedTypeTreeApi extends TypTreeApi { this: AppliedTypeTree =>
    val tpt: Tree
    val args: List[Tree]
  }

  override type TypeBoundsTree >: Null <: TypTree with TypeBoundsTreeApi

  /** The API that all type bound trees support */
  trait TypeBoundsTreeApi extends TypTreeApi { this: TypeBoundsTree =>
    val lo: Tree
    val hi: Tree
  }

  override type ExistentialTypeTree >: Null <: TypTree with ExistentialTypeTreeApi

  /** The API that all existential type trees support */
  trait ExistentialTypeTreeApi extends TypTreeApi { this: ExistentialTypeTree =>
    val tpt: Tree
    val whereClauses: List[Tree]
  }

  override type TypeTree >: Null <: TypTree with TypeTreeApi

  /** The API that all type trees support */
  trait TypeTreeApi extends TypTreeApi { this: TypeTree =>
    def original: Tree
  }

  /** An empty deferred value definition corresponding to:
   *    val _: _
   *  This is used as a placeholder in the `self` parameter Template if there is
   *  no definition of a self value of self type.
   */
  val emptyValDef: ValDef

// ---------------------- copying ------------------------------------------------

  /** The standard (lazy) tree copier
   */
  type TreeCopier <: TreeCopierOps
  val treeCopy: TreeCopier = newLazyTreeCopier

  def newStrictTreeCopier: TreeCopier
  def newLazyTreeCopier: TreeCopier

  /** The API of a tree copier
   *  tree copiers are made available by an implicit conversion in reflect.ops
   */
  abstract class TreeCopierOps {
    def ClassDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], impl: Template): ClassDef
    def PackageDef(tree: Tree, pid: RefTree, stats: List[Tree]): PackageDef
    def ModuleDef(tree: Tree, mods: Modifiers, name: Name, impl: Template): ModuleDef
    def ValDef(tree: Tree, mods: Modifiers, name: Name, tpt: Tree, rhs: Tree): ValDef
    def DefDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree): DefDef
    def TypeDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], rhs: Tree): TypeDef
    def LabelDef(tree: Tree, name: Name, params: List[Ident], rhs: Tree): LabelDef
    def Import(tree: Tree, expr: Tree, selectors: List[ImportSelector]): Import
    def Template(tree: Tree, parents: List[Tree], self: ValDef, body: List[Tree]): Template
    def Block(tree: Tree, stats: List[Tree], expr: Tree): Block
    def CaseDef(tree: Tree, pat: Tree, guard: Tree, body: Tree): CaseDef
    def Alternative(tree: Tree, trees: List[Tree]): Alternative
    def Star(tree: Tree, elem: Tree): Star
    def Bind(tree: Tree, name: Name, body: Tree): Bind
    def UnApply(tree: Tree, fun: Tree, args: List[Tree]): UnApply
    def ArrayValue(tree: Tree, elemtpt: Tree, trees: List[Tree]): ArrayValue
    def Function(tree: Tree, vparams: List[ValDef], body: Tree): Function
    def Assign(tree: Tree, lhs: Tree, rhs: Tree): Assign
    def AssignOrNamedArg(tree: Tree, lhs: Tree, rhs: Tree): AssignOrNamedArg
    def If(tree: Tree, cond: Tree, thenp: Tree, elsep: Tree): If
    def Match(tree: Tree, selector: Tree, cases: List[CaseDef]): Match
    def Return(tree: Tree, expr: Tree): Return
    def Try(tree: Tree, block: Tree, catches: List[CaseDef], finalizer: Tree): Try
    def Throw(tree: Tree, expr: Tree): Throw
    def New(tree: Tree, tpt: Tree): New
    def Typed(tree: Tree, expr: Tree, tpt: Tree): Typed
    def TypeApply(tree: Tree, fun: Tree, args: List[Tree]): TypeApply
    def Apply(tree: Tree, fun: Tree, args: List[Tree]): Apply
    def ApplyDynamic(tree: Tree, qual: Tree, args: List[Tree]): ApplyDynamic
    def Super(tree: Tree, qual: Tree, mix: TypeName): Super
    def This(tree: Tree, qual: Name): This
    def Select(tree: Tree, qualifier: Tree, selector: Name): Select
    def Ident(tree: Tree, name: Name): Ident
    def ReferenceToBoxed(tree: Tree, idt: Ident): ReferenceToBoxed
    def Literal(tree: Tree, value: Constant): Literal
    def TypeTree(tree: Tree): TypeTree
    def Annotated(tree: Tree, annot: Tree, arg: Tree): Annotated
    def SingletonTypeTree(tree: Tree, ref: Tree): SingletonTypeTree
    def SelectFromTypeTree(tree: Tree, qualifier: Tree, selector: Name): SelectFromTypeTree
    def CompoundTypeTree(tree: Tree, templ: Template): CompoundTypeTree
    def AppliedTypeTree(tree: Tree, tpt: Tree, args: List[Tree]): AppliedTypeTree
    def TypeBoundsTree(tree: Tree, lo: Tree, hi: Tree): TypeBoundsTree
    def ExistentialTypeTree(tree: Tree, tpt: Tree, whereClauses: List[Tree]): ExistentialTypeTree
  }

// ---------------------- traversing and transforming ------------------------------

  class Traverser {
    protected[scala] var currentOwner: Symbol = rootMirror.RootClass

    def traverse(tree: Tree): Unit = itraverse(this, tree)

    def traverseTrees(trees: List[Tree]) {
      trees foreach traverse
    }
    def traverseTreess(treess: List[List[Tree]]) {
      treess foreach traverseTrees
    }
    def traverseStats(stats: List[Tree], exprOwner: Symbol) {
      stats foreach (stat =>
        if (exprOwner != currentOwner) atOwner(exprOwner)(traverse(stat))
        else traverse(stat)
      )
    }

    def atOwner(owner: Symbol)(traverse: => Unit) {
      val prevOwner = currentOwner
      currentOwner = owner
      traverse
      currentOwner = prevOwner
    }

    /** Leave apply available in the generic traverser to do something else.
     */
    def apply[T <: Tree](tree: T): T = { traverse(tree); tree }
  }

  protected def itraverse(traverser: Traverser, tree: Tree): Unit = throw new MatchError(tree)

  protected def xtraverse(traverser: Traverser, tree: Tree): Unit = throw new MatchError(tree)

  abstract class Transformer {
    val treeCopy: TreeCopier = newLazyTreeCopier
    protected[scala] var currentOwner: Symbol = rootMirror.RootClass
    protected def currentMethod = currentOwner.enclosingMethod
    protected def currentClass = currentOwner.enclosingClass
//    protected def currentPackage = currentOwner.enclosingTopLevelClass.owner
    def transform(tree: Tree): Tree = itransform(this, tree)

    def transformTrees(trees: List[Tree]): List[Tree] =
        trees mapConserve (transform(_))
    def transformTemplate(tree: Template): Template =
      transform(tree: Tree).asInstanceOf[Template]
    def transformTypeDefs(trees: List[TypeDef]): List[TypeDef] =
      trees mapConserve (tree => transform(tree).asInstanceOf[TypeDef])
    def transformValDef(tree: ValDef): ValDef =
      if (tree.isEmpty) tree else transform(tree).asInstanceOf[ValDef]
    def transformValDefs(trees: List[ValDef]): List[ValDef] =
      trees mapConserve (transformValDef(_))
    def transformValDefss(treess: List[List[ValDef]]): List[List[ValDef]] =
      treess mapConserve (transformValDefs(_))
    def transformCaseDefs(trees: List[CaseDef]): List[CaseDef] =
      trees mapConserve (tree => transform(tree).asInstanceOf[CaseDef])
    def transformIdents(trees: List[Ident]): List[Ident] =
      trees mapConserve (tree => transform(tree).asInstanceOf[Ident])
    def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] =
      stats mapConserve (stat =>
        if (exprOwner != currentOwner && stat.isTerm) atOwner(exprOwner)(transform(stat))
        else transform(stat)) filter (EmptyTree != _)
    def transformModifiers(mods: Modifiers): Modifiers =
      mods.mapAnnotations(transformTrees)

    def atOwner[A](owner: Symbol)(trans: => A): A = {
      val prevOwner = currentOwner
      currentOwner = owner
      val result = trans
      currentOwner = prevOwner
      result
    }
  }

  protected def itransform(transformer: Transformer, tree: Tree): Tree = throw new MatchError(tree)

  protected def xtransform(transformer: Transformer, tree: Tree): Tree = throw new MatchError(tree)

  type Modifiers >: Null <: ModifiersApi

  abstract class ModifiersApi extends ModifiersBase with HasFlagsApi

}

