/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala
package reflect
package api

import scala.annotation.{nowarn, tailrec}

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 * This trait defines the node types used in Scala abstract syntax trees (AST) and operations on them.
 *
 * Trees are the basis for Scala's abstract syntax that is used to represent programs. They are also called
 * abstract syntax trees and commonly abbreviated as ASTs.
 *
 * In Scala reflection, APIs that produce or use `Tree`s are:
 *
 *   - '''Annotations''' which use trees to represent their arguments, exposed in [[scala.reflect.api.Annotations.AnnotationApi#scalaArgs Annotation.scalaArgs]].
 *   - '''[[scala.reflect.api.Universe#reify reify]]''', a special method on [[scala.reflect.api.Universe]] that takes an expression and returns an AST which represents the expression.
 *   - '''Macros and runtime compilation with toolboxes''' which both use trees as their program representation medium.
 *
 *  Trees are immutable, except for three fields
 *  [[Trees#TreeApi.pos pos]], [[Trees#TreeApi.symbol symbol]], and [[Trees#TreeApi.tpe tpe]], which are assigned when a tree is typechecked
 *  to attribute it with the information gathered by the typechecker.
 *
 *  === Examples ===
 *
 *  The following creates an AST representing a literal 5 in Scala source code:
 *  {{{
 *    Literal(Constant(5))
 *  }}}
 *
 *  The following creates an AST representing `print("Hello World")`:
 *  {{{
 *    Apply(Select(Select(This(TypeName("scala")), TermName("Predef")), TermName("print")), List(Literal(Constant("Hello World"))))
 *  }}}
 *
 *  The following creates an AST from a literal 5, and then uses `showRaw` to print it in a readable format.
 *  {{{
 *    import scala.reflect.runtime.universe.{ reify, showRaw }
 *    print( showRaw( reify{5}.tree ) )` // prints Literal(Constant(5))
 *  }}}
 *
 *  For more information about `Tree`s, see the [[https://docs.scala-lang.org/overviews/reflection/symbols-trees-types.html Reflection Guide: Symbols, Trees, Types]].
 *
 *  @groupname Traversal Tree Traversal and Transformation
 *  @groupprio Traversal 1
 *  @groupprio Factories 1
 *  @groupname Copying   Tree Copying
 *  @groupprio Copying   1
 *
 *  @contentDiagram hideNodes "*Api"
 *  @group ReflectionAPI
 */
trait Trees { self: Universe =>

  /** The type of Scala abstract syntax trees.
   *  @group Trees
   *  @template
   */
  type Tree >: Null <: AnyRef with TreeApi

  /** The API that all trees support.
   *  The main source of information about trees is the [[scala.reflect.api.Trees]] page.
   *  @group API
   */
  trait TreeApi extends Product { this: Tree =>
    /** Does this tree represent a definition? (of a method, of a class, etc) */
    def isDef: Boolean

    /** Is this tree one of the empty trees?
     *
     *  Empty trees are: the `EmptyTree` null object and `TypeTree` instances that don't carry a type.
     *
     *  @see `canHaveAttrs`
     */
    def isEmpty: Boolean

    /** Is this tree not an empty tree?
     *
     *  @see `isEmpty`
     */
    def nonEmpty: Boolean

    /** Can this tree carry attributes (i.e. symbols, types or positions)?
     *  Typically the answer is yes, except for the `EmptyTree` null object and
     *  two special singletons: `noSelfType` and `pendingSuperCall`.
     */
    def canHaveAttrs: Boolean

    /** The canonical way to test if a Tree represents a term.
     */
    def isTerm: Boolean

    /** The canonical way to test if a Tree represents a type.
     */
    def isType: Boolean

    /** Position of the tree. */
    def pos: Position

    /** Type of the tree.
     *
     *  Upon creation most trees have their `tpe` set to `null`.
     *  Types are typically assigned to trees during typechecking.
     *  Some node factory methods set `tpe` immediately after creation.
     *
     *  When the typechecker encounters a tree with a non-null tpe,
     *  it will assume it to be correct and not check it again. This means one has
     *  to be careful not to erase the `tpe` field of subtrees.
     */
    def tpe: Type

    /** Symbol of the tree.
     *
     *  For most trees symbol is `null`. In `SymTree`s,
     *  it is overridden and implemented with a var, initialized to `NoSymbol`.
     *
     *  Trees which are not `SymTree`s but which carry symbols do so by
     *  overriding `def symbol` to forward it elsewhere.  Examples:
     *
     *    - `Super(qual, _)`              has `qual`'s symbol,
     *    - `Apply(fun, args)`            has `fun`'s symbol,
     *    - `TypeApply(fun, args)`        has `fun`'s symbol,
     *    - `AppliedTypeTree(tpt, args)`  has `tpt`'s symbol,
     *    - `TypeTree(tpe)`               has `tpe`'s `typeSymbol`, if `tpe != null`.
     */
    def symbol: Symbol

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

    /** Apply `pf` to each subtree on which the function is defined and collect the results.
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

    /** Tests whether two trees are structurally equal.
     *  Note that `==` on trees is reference equality.
     */
    def equalsStructure(that : Tree): Boolean

    /** The direct child trees of this tree.
     *  EmptyTrees are always omitted.  Lists are flattened.
     */
    def children: List[Tree]

    /** Make a copy of this tree, keeping all attributes,
     *  except that all positions are focused (so nothing
     *  in this tree will be found when searching by position).
     */
    def duplicate: this.type

    /** Obtains string representation of a tree */
    override def toString: String = treeToString(this)
  }

  /** Obtains string representation of a tree
   *  @group Trees
   */
  protected def treeToString(tree: Tree): String

  /** The empty tree
   *  @group Trees
   */
  val EmptyTree: Tree

  /** A tree for a term.  Not all trees representing terms are TermTrees; use isTerm
   *  to reliably identify terms.
   *  @group Trees
   *  @template
   */
  type TermTree >: Null <: TermTreeApi with Tree

  /** The API that all term trees support
   *  @group API
   */
  trait TermTreeApi extends TreeApi { this: TermTree =>
  }

  /** A tree for a type. Not all trees representing types are TypTrees; use isType
   *  to reliably identify types.
   *  @group Trees
   *  @template
   */
  type TypTree >: Null <: TypTreeApi with Tree

  /** The API that all typ trees support
   *  @group API
   */
  trait TypTreeApi extends TreeApi { this: TypTree =>
  }

  /** A tree that carries a symbol, e.g. by defining it (`DefTree`) or by referring to it (`RefTree`).
   *  Such trees start their life naked, returning `NoSymbol`, but after being typechecked without errors
   *  they hold non-empty symbols.
   *
   *  @group Trees
   *  @template
   */
  type SymTree >: Null <: SymTreeApi with Tree

  /** The API that all sym trees support
   *  @group API
   */
  trait SymTreeApi extends TreeApi { this: SymTree =>
    /** @inheritdoc */
    def symbol: Symbol
  }

  /** A tree that carries a name, e.g. by defining it (`DefTree`) or by referring to it (`RefTree`).
   *  @group Trees
   *  @template
   */
  type NameTree >: Null <: NameTreeApi with Tree

  /** The API that all name trees support
   *  @group API
   */
  trait NameTreeApi extends TreeApi { this: NameTree =>
    /** The underlying name.
     *  For example, the `List` part of `Ident(TermName("List"))`.
     */
    def name: Name

    /** Position of the subtree bearing the name. */
    def namePos: Position
  }

  /** A tree which references a symbol-carrying entity.
   *  References one, as opposed to defining one; definitions
   *  are in DefTrees.
   *  @group Trees
   *  @template
   */
  type RefTree >: Null <: RefTreeApi with SymTree with NameTree

  /** The API that all ref trees support
   *  @group API
   */
  trait RefTreeApi extends SymTreeApi with NameTreeApi { this: RefTree =>
    /** The qualifier of the reference.
     *  For example, the `Ident(TermName("scala"))` part of `Select(Ident(TermName("scala")), TermName("List"))`.
     *  `EmptyTree` for `Ident` instances.
     */
    def qualifier: Tree

    /** @inheritdoc */
    def name: Name
  }

  /** The constructor/extractor for `RefTree` instances.
   *  @group Extractors
   */
  val RefTree: RefTreeExtractor

  /** An extractor class to create and pattern match with syntax `RefTree(qual, name)`.
   *  This AST node corresponds to either Ident, Select or SelectFromTypeTree.
   *  @group Extractors
   */
  abstract class RefTreeExtractor {
    def apply(qualifier: Tree, name: Name): RefTree
    def unapply(refTree: RefTree): Option[(Tree, Name)]
  }

  /** A tree representing a symbol-defining entity:
   *    1) A declaration or a definition (type, class, object, package, val, var, or def)
   *    2) `Bind` that is used to represent binding occurrences in pattern matches
   *    3) `LabelDef` that is used internally to represent while loops
   *  @group Trees
   *  @template
   */
  type DefTree >: Null <: DefTreeApi with SymTree with NameTree

  /** The API that all def trees support
   *  @group API
   */
  trait DefTreeApi extends SymTreeApi with NameTreeApi { this: DefTree =>
    /** @inheritdoc */
    def name: Name
  }

  /** Common base class for all member definitions: types, classes,
   *  objects, packages, vals and vars, defs.
   *  @group Trees
   *  @template
   */
  type MemberDef >: Null <: MemberDefApi with DefTree

  /** The API that all member defs support
   *  @group API
   */
  trait MemberDefApi extends DefTreeApi { this: MemberDef =>
    /** Modifiers of the declared member. */
    def mods: Modifiers
  }

  /** A packaging, such as `package pid { stats }`
   *  @group Trees
   *  @template
   */
  type PackageDef >: Null <: PackageDefApi with MemberDef

  /** The constructor/extractor for `PackageDef` instances.
   *  @group Extractors
   */
  val PackageDef: PackageDefExtractor

  /** An extractor class to create and pattern match with syntax `PackageDef(pid, stats)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    `package` pid { stats }
   *  @group Extractors
   */
  abstract class PackageDefExtractor {
    def apply(pid: RefTree, stats: List[Tree]): PackageDef
    def unapply(packageDef: PackageDef): Option[(RefTree, List[Tree])]
  }

  /** The API that all package defs support
   *  @group API
   */
  trait PackageDefApi extends MemberDefApi { this: PackageDef =>
    /** The (possibly, fully-qualified) name of the package. */
    def pid: RefTree

    /** Body of the package definition. */
    def stats: List[Tree]
  }

  /** A common base class for class and object definitions.
   *  @group Trees
   *  @template
   */
  type ImplDef >: Null <: ImplDefApi with MemberDef

  /** The API that all impl defs support
   *  @group API
   */
  trait ImplDefApi extends MemberDefApi { this: ImplDef =>
    /** The body of the definition. */
    def impl: Template
  }

  /** A class definition.
   *  @group Trees
   *  @template
   */
  type ClassDef >: Null <: ClassDefApi with ImplDef

  /** The constructor/extractor for `ClassDef` instances.
   *  @group Extractors
   */
  val ClassDef: ClassDefExtractor

  /** An extractor class to create and pattern match with syntax `ClassDef(mods, name, tparams, impl)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    mods `class` name [tparams] impl
   *
   *  Where impl stands for:
   *
   *    `extends` parents { defs }
   *  @group Extractors
   */
  abstract class ClassDefExtractor {
    def apply(mods: Modifiers, name: TypeName, tparams: List[TypeDef], impl: Template): ClassDef
    def unapply(classDef: ClassDef): Option[(Modifiers, TypeName, List[TypeDef], Template)]

    /** @see [[Internals.InternalApi.classDef]] */
    @deprecated("use `internal.classDef` instead", "2.11.0")
    def apply(sym: Symbol, impl: Template)(implicit token: CompatToken): ClassDef = internal.classDef(sym, impl)
  }

  /** The API that all class defs support
   *  @group API
   */
  trait ClassDefApi extends ImplDefApi { this: ClassDef =>
    /** @inheritdoc */
    def mods: Modifiers

    /** The name of the class. */
    def name: TypeName

    /** The type parameters of the class. */
    def tparams: List[TypeDef]

    /** @inheritdoc */
    def impl: Template
  }

  /** An object definition, e.g. `object Foo`.  Internally, objects are
   *  quite frequently called modules to reduce ambiguity.
   *  Eliminated by compiler phase refcheck.
   *  @group Trees
   *  @template
   */
  type ModuleDef >: Null <: ModuleDefApi with ImplDef

  /** The constructor/extractor for `ModuleDef` instances.
   *  @group Extractors
   */
  val ModuleDef: ModuleDefExtractor

  /** An extractor class to create and pattern match with syntax `ModuleDef(mods, name, impl)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    mods `object` name impl
   *
   *  Where impl stands for:
   *
   *    `extends` parents { defs }
   *  @group Extractors
   */
  abstract class ModuleDefExtractor {
    def apply(mods: Modifiers, name: TermName, impl: Template): ModuleDef
    def unapply(moduleDef: ModuleDef): Option[(Modifiers, TermName, Template)]

    /** @see [[Internals.InternalApi.moduleDef]] */
    @deprecated("use `internal.moduleDef` instead", "2.11.0")
    def apply(sym: Symbol, impl: Template)(implicit token: CompatToken): ModuleDef = internal.moduleDef(sym, impl)
  }

  /** The API that all module defs support
   *  @group API
   */
  trait ModuleDefApi extends ImplDefApi { this: ModuleDef =>
    /** @inheritdoc */
    def mods: Modifiers

    /** The name of the module. */
    def name: TermName

    /** @inheritdoc */
    def impl: Template
  }

  /** A common base class for ValDefs and DefDefs.
   *  @group Trees
   *  @template
   */
  type ValOrDefDef >: Null <: ValOrDefDefApi with MemberDef

  /** The API that all val defs and def defs support
   *  @group API
   */
  trait ValOrDefDefApi extends MemberDefApi { this: ValOrDefDef =>
    /** @inheritdoc */
    def name: TermName

    /** The type ascribed to the definition.
     *  An empty `TypeTree` if the type hasn't been specified explicitly
     *  and is supposed to be inferred.
     */
    def tpt: Tree

    /** The body of the definition.
     *  The `EmptyTree` is the body is empty (e.g. for abstract members).
     */
    def rhs: Tree
  }

  /** Broadly speaking, a value definition.  All these are encoded as ValDefs:
   *
   *   - immutable values, e.g. "val x"
   *   - mutable values, e.g. "var x" - the MUTABLE flag set in mods
   *   - lazy values, e.g. "lazy val x" - the LAZY flag set in mods
   *   - method parameters, see vparamss in [[scala.reflect.api.Trees#DefDef]] - the PARAM flag is set in mods
   *   - explicit self-types, e.g. class A { self: Bar => }
   *  @group Trees
   *  @template
   */
  type ValDef >: Null <: ValDefApi with ValOrDefDef

  /** The constructor/extractor for `ValDef` instances.
   *  @group Extractors
   */
  val ValDef: ValDefExtractor

  /** An extractor class to create and pattern match with syntax `ValDef(mods, name, tpt, rhs)`.
   *  This AST node corresponds to any of the following Scala code:
   *
   *    mods `val` name: tpt = rhs
   *
   *    mods `var` name: tpt = rhs
   *
   *    mods name: tpt = rhs        // in signatures of function and method definitions
   *
   *    self: Bar =>                // self-types
   *
   *  If the type of a value is not specified explicitly (i.e. is meant to be inferred),
   *  this is expressed by having `tpt` set to `TypeTree()` (but not to an `EmptyTree`!).
   *  @group Extractors
   */
  abstract class ValDefExtractor {
    def apply(mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree): ValDef
    def unapply(valDef: ValDef): Option[(Modifiers, TermName, Tree, Tree)]

    /** @see [[Internals.InternalApi.valDef]] */
    @deprecated("use `internal.valDef` instead", "2.11.0")
    def apply(sym: Symbol, rhs: Tree)(implicit token: CompatToken): ValDef = internal.valDef(sym, rhs)

    /** @see [[Internals.InternalApi.valDef]] */
    @deprecated("use `internal.valDef` instead", "2.11.0")
    def apply(sym: Symbol)(implicit token: CompatToken): ValDef = internal.valDef(sym)
  }

  /** The API that all val defs support
   *  @group API
   */
  trait ValDefApi extends ValOrDefDefApi { this: ValDef =>
    /** @inheritdoc */
    def mods: Modifiers

    /** @inheritdoc */
    def name: TermName

    /** @inheritdoc */
    def tpt: Tree

    /** @inheritdoc */
    def rhs: Tree
  }

  /** A method or macro definition.
   *  @param name   The name of the method or macro. Can be a type name in case this is a type macro
   *  @group Trees
   *  @template
   */
  type DefDef >: Null <: DefDefApi with ValOrDefDef

  /** The constructor/extractor for `DefDef` instances.
   *  @group Extractors
   */
  val DefDef: DefDefExtractor

  /** An extractor class to create and pattern match with syntax `DefDef(mods, name, tparams, vparamss, tpt, rhs)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    mods `def` name[tparams](vparams_1)...(vparams_n): tpt = rhs
   *
   *  If the return type is not specified explicitly (i.e. is meant to be inferred),
   *  this is expressed by having `tpt` set to `TypeTree()` (but not to an `EmptyTree`!).
   *  @group Extractors
   */
  abstract class DefDefExtractor {
    def apply(mods: Modifiers, name: TermName, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree): DefDef
    def unapply(defDef: DefDef): Option[(Modifiers, TermName, List[TypeDef], List[List[ValDef]], Tree, Tree)]

    /** @see [[Internals.InternalApi.defDef]] */
    @deprecated("use `internal.defDef` instead", "2.11.0")
    def apply(sym: Symbol, mods: Modifiers, vparamss: List[List[ValDef]], rhs: Tree)(implicit token: CompatToken): DefDef = internal.defDef(sym, mods, vparamss, rhs)

    /** @see [[Internals.InternalApi.defDef]] */
    @deprecated("use `internal.defDef` instead", "2.11.0")
    def apply(sym: Symbol, vparamss: List[List[ValDef]], rhs: Tree)(implicit token: CompatToken): DefDef = internal.defDef(sym, vparamss, rhs)

    /** @see [[Internals.InternalApi.defDef]] */
    @deprecated("use `internal.defDef` instead", "2.11.0")
    def apply(sym: Symbol, mods: Modifiers, rhs: Tree)(implicit token: CompatToken): DefDef = internal.defDef(sym, mods, rhs)

    /** @see [[Internals.InternalApi.defDef]] */
    @deprecated("use `internal.defDef` instead", "2.11.0")
    def apply(sym: Symbol, rhs: Tree)(implicit token: CompatToken): DefDef = internal.defDef(sym, rhs)

    /** @see [[Internals.InternalApi.defDef]] */
    @deprecated("use `internal.defDef` instead", "2.11.0")
    def apply(sym: Symbol, rhs: List[List[Symbol]] => Tree)(implicit token: CompatToken): DefDef = internal.defDef(sym, rhs)
  }

  /** The API that all def defs support
   *  @group API
   */
  trait DefDefApi extends ValOrDefDefApi { this: DefDef =>
    /** @inheritdoc */
    def mods: Modifiers

    /** @inheritdoc */
    def name: TermName

    /** The type parameters of the method. */
    def tparams: List[TypeDef]

    /** The parameter lists of the method. */
    def vparamss: List[List[ValDef]]

    /** @inheritdoc */
    def tpt: Tree

    /** @inheritdoc */
    def rhs: Tree
  }

  /** An abstract type, a type parameter, or a type alias.
   *  Eliminated by erasure.
   *  @group Trees
   *  @template
   */
  type TypeDef >: Null <: TypeDefApi with MemberDef

  /** The constructor/extractor for `TypeDef` instances.
   *  @group Extractors
   */
  val TypeDef: TypeDefExtractor

  /** An extractor class to create and pattern match with syntax `TypeDef(mods, name, tparams, rhs)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    mods `type` name[tparams] = rhs
   *
   *    mods `type` name[tparams] >: lo <: hi
   *
   *  First usage illustrates `TypeDefs` representing type aliases and type parameters.
   *  Second usage illustrates `TypeDefs` representing abstract types,
   *  where lo and hi are both `TypeBoundsTrees` and `Modifier.deferred` is set in mods.
   *  @group Extractors
   */
  abstract class TypeDefExtractor {
    def apply(mods: Modifiers, name: TypeName, tparams: List[TypeDef], rhs: Tree): TypeDef
    def unapply(typeDef: TypeDef): Option[(Modifiers, TypeName, List[TypeDef], Tree)]

    /** @see [[Internals.InternalApi.typeDef]] */
    @deprecated("use `internal.typeDef` instead", "2.11.0")
    def apply(sym: Symbol, rhs: Tree)(implicit token: CompatToken): TypeDef = internal.typeDef(sym, rhs)

    /** @see [[Internals.InternalApi.typeDef]] */
    @deprecated("use `internal.typeDef` instead", "2.11.0")
    def apply(sym: Symbol)(implicit token: CompatToken): TypeDef = internal.typeDef(sym)
  }

  /** The API that all type defs support
   *  @group API
   */
  trait TypeDefApi extends MemberDefApi { this: TypeDef =>
    /** @inheritdoc */
    def mods: Modifiers

    /** @inheritdoc */
    def name: TypeName

    /** The type parameters of this type definition. */
    def tparams: List[TypeDef]

    /** The body of the definition.
     *  The `EmptyTree` is the body is empty (e.g. for abstract type members).
     */
    def rhs: Tree
  }

  /** A labelled expression.  Not expressible in language syntax, but
   *  generated by the compiler to simulate while/do-while loops, and
   *  also by the pattern matcher.
   *
   *  The label acts much like a nested function, where `params` represents
   *  the incoming parameters.  The symbol given to the LabelDef should have
   *  a MethodType, as if it were a nested function.
   *
   *  Jumps are apply nodes attributed with a label's symbol.  The
   *  arguments from the apply node will be passed to the label and
   *  assigned to the Idents.
   *
   *  Forward jumps within a block are allowed.
   *  @group Trees
   *  @template
   */
  type LabelDef >: Null <: LabelDefApi with DefTree with TermTree

  /** The constructor/extractor for `LabelDef` instances.
   *  @group Extractors
   */
  val LabelDef: LabelDefExtractor

  /** An extractor class to create and pattern match with syntax `LabelDef(name, params, rhs)`.
   *
   *  This AST node does not have direct correspondence to Scala code.
   *  It is used for tailcalls and like.
   *  For example, while/do are desugared to label defs as follows:
   *  {{{
   *    while (cond) body ==> LabelDef(\$L, List(), if (cond) { body; L\$() } else ())
   *  }}}
   *  {{{
   *    do body while (cond) ==> LabelDef(\$L, List(), body; if (cond) L\$() else ())
   *  }}}
   *  @group Extractors
   */
  abstract class LabelDefExtractor {
    def apply(name: TermName, params: List[Ident], rhs: Tree): LabelDef
    def unapply(labelDef: LabelDef): Option[(TermName, List[Ident], Tree)]

    /** @see [[Internals.InternalApi.labelDef]] */
    @deprecated("use `internal.labelDef` instead", "2.11.0")
    def apply(sym: Symbol, params: List[Symbol], rhs: Tree)(implicit token: CompatToken): LabelDef = internal.labelDef(sym, params, rhs)
  }

  /** The API that all label defs support
   *  @group API
   */
  trait LabelDefApi extends DefTreeApi with TermTreeApi { this: LabelDef =>
    /** @inheritdoc */
    def name: TermName

    /** Label's parameters - names that can be used in the body of the label.
     *  See the example for [[scala.reflect.api.Trees#LabelDefExtractor]].
     */
    def params: List[Ident]

    /** The body of the label.
     *  See the example for [[scala.reflect.api.Trees#LabelDefExtractor]].
     */
    def rhs: Tree
  }

  /** Import selector (not a tree, but a component of the `Import` tree)
   *
   *  Representation of an imported name its optional rename and their optional positions
   *
   *  Eliminated by typecheck.
   *
   * @param name      the imported name
   * @param namePos   its position or -1 if undefined
   * @param rename    the name the import is renamed to (== name if no renaming)
   * @param renamePos the position of the rename or -1 if undefined
   *  @group Trees
   *  @template
   */
  type ImportSelector >: Null <: AnyRef with ImportSelectorApi

  /** The constructor/extractor for `ImportSelector` instances.
   *  @group Extractors
   */
  val ImportSelector: ImportSelectorExtractor

  /** An extractor class to create and pattern match with syntax `ImportSelector(name, namePos, rename, renamePos)`.
   *  This is not an AST node, it is used as a part of the `Import` node.
   *  @group Extractors
   */
  abstract class ImportSelectorExtractor {
    def apply(name: Name, namePos: Int, rename: Name, renamePos: Int): ImportSelector
    def unapply(importSelector: ImportSelector): Option[(Name, Int, Name, Int)]
  }

  /** The API that all import selectors support
   *  @group API
   */
  trait ImportSelectorApi { this: ImportSelector =>
    /** The imported name. */
    def name: Name

    /** Offset of the position of the importing part of the selector in the source file.
     *  Is equal to -1 is the position is unknown.
     */
    def namePos: Int

    /** The name the import is renamed to.
     *  Is equal to `name` if it's not a renaming import.
     */
    def rename: Name

    /** Offset of the position of the renaming part of the selector in the source file.
     *  Is equal to -1 is the position is unknown.
     */
    def renamePos: Int

    /** Does the selector mask or hide a name? `import x.{y => _}` */
    def isMask: Boolean

    /** Does the selector introduce a specific name? `import a.b, x.{y => z}` */
    def isSpecific: Boolean

    /** Does the selector introduce a specific name by rename? `x.{y => z}` */
    def isRename: Boolean

    /** Is the selector a wildcard import that introduces all available names? `import x._` */
    def isWildcard: Boolean
  }

  /** Import clause
   *
   *  @param expr
   *  @param selectors
   *  @group Trees
   *  @template
   */
  type Import >: Null <: ImportApi with SymTree

  /** The constructor/extractor for `Import` instances.
   *  @group Extractors
   */
  val Import: ImportExtractor

  /** An extractor class to create and pattern match with syntax `Import(expr, selectors)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    import expr.{selectors}
   *
   *  Selectors are a list of ImportSelectors, which conceptually are pairs of names (from, to).
   *  The last (and maybe only name) may be a nme.WILDCARD. For instance:
   *
   *    import qual.{w => _, x, y => z, _}
   *
   *  Would be represented as:
   *
   *    Import(qual, List(("w", WILDCARD), ("x", "x"), ("y", "z"), (WILDCARD, null)))
   *
   *  The symbol of an `Import` is an import symbol @see Symbol.newImport.
   *  It's used primarily as a marker to check that the import has been typechecked.
   *  @group Extractors
   */
  abstract class ImportExtractor {
    def apply(expr: Tree, selectors: List[ImportSelector]): Import
    def unapply(import_ : Import): Option[(Tree, List[ImportSelector])]
  }

  /** The API that all imports support
   *  @group API
   */
  trait ImportApi extends SymTreeApi { this: Import =>
    /** The qualifier of the import.
     *  See the example for [[scala.reflect.api.Trees#ImportExtractor]].
     */
    def expr: Tree

    /** The selectors of the import.
     *  See the example for [[scala.reflect.api.Trees#ImportExtractor]].
     */
    def selectors: List[ImportSelector]
  }

  /** Instantiation template of a class or trait
   *
   *  @param parents
   *  @param body
   *  @group Trees
   *  @template
   */
  type Template >: Null <: TemplateApi with SymTree

  /** The constructor/extractor for `Template` instances.
   *  @group Extractors
   */
  val Template: TemplateExtractor

  /** An extractor class to create and pattern match with syntax `Template(parents, self, body)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    `extends` parents { self => body }
   *
   *  In case when the self-type annotation is missing, it is represented as
   *  an empty value definition with nme.WILDCARD as name and NoType as type.
   *
   *  The symbol of a template is a local dummy. @see Symbol.newLocalDummy
   *  The owner of the local dummy is the enclosing trait or class.
   *  The local dummy is itself the owner of any local blocks. For example:
   *
   *    class C {
   *      def foo { // owner is C
   *        def bar  // owner is local dummy
   *      }
   *    }
   *  @group Extractors
   */
  abstract class TemplateExtractor {
    def apply(parents: List[Tree], self: ValDef, body: List[Tree]): Template
    def unapply(template: Template): Option[(List[Tree], ValDef, List[Tree])]
  }

  /** The API that all templates support
   *  @group API
   */
  trait TemplateApi extends SymTreeApi { this: Template =>
    /** Superclasses of the template. */
    def parents: List[Tree]

    /** Self type of the template.
     *  Is equal to `noSelfType` if the self type is not specified.
     */
    def self: ValDef

    /** Body of the template.
     */
    def body: List[Tree]
  }

  /** Block of expressions (semicolon separated expressions)
   *  @group Trees
   *  @template
   */
  type Block >: Null <: BlockApi with TermTree

  /** The constructor/extractor for `Block` instances.
   *  @group Extractors
   */
  val Block: BlockExtractor

  /** An extractor class to create and pattern match with syntax `Block(stats, expr)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    { stats; expr }
   *
   *  If the block is empty, the `expr` is set to `Literal(Constant(()))`.
   *  @group Extractors
   */
  abstract class BlockExtractor {
    def apply(stats: List[Tree], expr: Tree): Block
    def unapply(block: Block): Option[(List[Tree], Tree)]
  }

  /** The API that all blocks support
   *  @group API
   */
  trait BlockApi extends TermTreeApi { this: Block =>
    /** All, but the last, expressions in the block.
     *  Can very well be an empty list.
     */
    def stats: List[Tree]

    /** The last expression in the block. */
    def expr: Tree
  }

  /** Case clause in a pattern match.
   *  (except for occurrences in switch statements).
   *  Eliminated by compiler phases patmat (in the new pattern matcher of 2.10) or explicitouter (in the old pre-2.10 pattern matcher)
   *  @group Trees
   *  @template
   */
  type CaseDef >: Null <: CaseDefApi with Tree

  /** The constructor/extractor for `CaseDef` instances.
   *  @group Extractors
   */
  val CaseDef: CaseDefExtractor

  /** An extractor class to create and pattern match with syntax `CaseDef(pat, guard, body)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    `case` pat `if` guard => body
   *
   *  If the guard is not present, the `guard` is set to `EmptyTree`.
   *  If the body is not specified, the `body` is set to `Literal(Constant(()))`
   *  @group Extractors
   */
  abstract class CaseDefExtractor {
    def apply(pat: Tree, guard: Tree, body: Tree): CaseDef
    def unapply(caseDef: CaseDef): Option[(Tree, Tree, Tree)]
  }

  /** The API that all case defs support
   *  @group API
   */
  trait CaseDefApi extends TreeApi { this: CaseDef =>
    /** The pattern of the pattern matching clause. */
    def pat: Tree

    /** The guard of the pattern matching clause.
     *  Is equal to `EmptyTree` if the guard is not specified.
     */
    def guard: Tree

    /** The body of the pattern matching clause.
     *  Is equal to `Literal(Constant(()))` if the body is not specified.
     */
    def body: Tree
  }

  /** Alternatives of patterns.
   *
   * Eliminated by compiler phases Eliminated by compiler phases patmat (in the new pattern matcher of 2.10) or explicitouter (in the old pre-2.10 pattern matcher),
   * except for
   *  occurrences in encoded Switch stmt (i.e. remaining Match(CaseDef(...)))
   *  @group Trees
   *  @template
   */
  type Alternative >: Null <: AlternativeApi with TermTree

  /** The constructor/extractor for `Alternative` instances.
   *  @group Extractors
   */
  val Alternative: AlternativeExtractor

  /** An extractor class to create and pattern match with syntax `Alternative(trees)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    pat1 | ... | patn
   *  @group Extractors
   */
  abstract class AlternativeExtractor {
    def apply(trees: List[Tree]): Alternative
    def unapply(alternative: Alternative): Option[List[Tree]]
  }

  /** The API that all alternatives support
   *  @group API
   */
  trait AlternativeApi extends TermTreeApi { this: Alternative =>
    /** Alternatives of the pattern matching clause. */
    def trees: List[Tree]
  }

  /** Repetition of pattern.
   *
   *  Eliminated by compiler phases patmat (in the new pattern matcher of 2.10) or explicitouter (in the old pre-2.10 pattern matcher).
   *  @group Trees
   *  @template
   */
  type Star >: Null <: StarApi with TermTree

  /** The constructor/extractor for `Star` instances.
   *  @group Extractors
   */
  val Star: StarExtractor

  /** An extractor class to create and pattern match with syntax `Star(elem)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    pat*
   *  @group Extractors
   */
  abstract class StarExtractor {
    def apply(elem: Tree): Star
    def unapply(star: Star): Option[Tree]
  }

  /** The API that all stars support
   *  @group API
   */
  trait StarApi extends TermTreeApi { this: Star =>
    /** The quantified pattern. */
    def elem: Tree
  }

  /** Bind a variable to a rhs pattern.
   *
   * Eliminated by compiler phases patmat (in the new pattern matcher of 2.10) or explicitouter (in the old pre-2.10 pattern matcher).
   *
   *  @param name
   *  @param body
   *  @group Trees
   *  @template
   */
  type Bind >: Null <: BindApi with DefTree

  /** The constructor/extractor for `Bind` instances.
   *  @group Extractors
   */
  val Bind: BindExtractor

  /** An extractor class to create and pattern match with syntax `Bind(name, body)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    pat*
   *  @group Extractors
   */
  abstract class BindExtractor {
    def apply(name: Name, body: Tree): Bind
    def unapply(bind: Bind): Option[(Name, Tree)]
  }

  /** The API that all binds support
   *  @group API
   */
  trait BindApi extends DefTreeApi { this: Bind =>
    /** The name that can be used to refer to this fragment of the matched expression.
     *  The `list` part of the `list @ List(x, y)`.
     */
    def name: Name

    /** The pattern that represents this fragment of the matched expression.
     *  The `List(x, y)` part of the `list @ List(x, y)`.
     *  Is equal to `EmptyTree` if the pattern is not specified as in `case x => x`.
     */
    def body: Tree
  }

  /**
   * Used to represent `unapply` methods in pattern matching.
   *
   *  For example:
   *  {{{
   *    2 match { case Foo(x) => x }
   *  }}}
   *
   *  Is represented as:
   *  {{{
   *    Match(
   *      Literal(Constant(2)),
   *      List(
   *        CaseDef(
   *          UnApply(
   *            // a dummy node that carries the type of unapplication to patmat
   *            // the <unapply-selector> here doesn't have an underlying symbol
   *            // it only has a type assigned, therefore after `untypecheck` this tree is no longer typeable
   *            Apply(Select(Ident(Foo), TermName("unapply")), List(Ident(TermName("<unapply-selector>")))),
   *            // arguments of the unapply => nothing synthetic here
   *            List(Bind(TermName("x"), Ident(nme.WILDCARD)))),
   *          EmptyTree,
   *          Ident(TermName("x")))))
   *  }}}
   *
   * Introduced by typer. Eliminated by compiler phases patmat (in the new pattern matcher of 2.10) or explicitouter (in the old pre-2.10 pattern matcher).
   *  @group Trees
   *  @template
   */
  type UnApply >: Null <: UnApplyApi with TermTree

  /** The constructor/extractor for `UnApply` instances.
   *  @group Extractors
   */
  val UnApply: UnApplyExtractor

  /** An extractor class to create and pattern match with syntax `UnApply(fun, args)`.
   *  This AST node does not have direct correspondence to Scala code,
   *  and is introduced when typechecking pattern matches and `try` blocks.
   *  @group Extractors
   */
  abstract class UnApplyExtractor {
    def apply(fun: Tree, args: List[Tree]): UnApply
    def unapply(unApply: UnApply): Option[(Tree, List[Tree])]
  }

  /** The API that all unapplies support
   *  @group API
   */
  trait UnApplyApi extends TermTreeApi { this: UnApply =>
    /** A dummy node that carries the type of unapplication.
     *  See the example for [[scala.reflect.api.Trees#UnApplyExtractor]].
     */
    def fun: Tree

    /** The arguments of the unapplication.
     *  See the example for [[scala.reflect.api.Trees#UnApplyExtractor]].
     */
    def args: List[Tree]
  }

  /** Anonymous function, eliminated by compiler phase lambdalift
   *  @group Trees
   *  @template
   */
  type Function >: Null <: FunctionApi with TermTree with SymTree

  /** The constructor/extractor for `Function` instances.
   *  @group Extractors
   */
  val Function: FunctionExtractor

  /** An extractor class to create and pattern match with syntax `Function(vparams, body)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    vparams => body
   *
   *  The symbol of a Function is a synthetic TermSymbol.
   *  It is the owner of the function's parameters.
   *  @group Extractors
   */
  abstract class FunctionExtractor {
    def apply(vparams: List[ValDef], body: Tree): Function
    def unapply(function: Function): Option[(List[ValDef], Tree)]
  }

  /** The API that all functions support
   *  @group API
   */
  trait FunctionApi extends TermTreeApi with SymTreeApi { this: Function =>
    /** The list of parameters of the function.
     */
    def vparams: List[ValDef]

    /** The body of the function.
     */
    def body: Tree
  }

  /** Assignment
   *  @group Trees
   *  @template
   */
  type Assign >: Null <: AssignApi with TermTree

  /** The constructor/extractor for `Assign` instances.
   *  @group Extractors
   */
  val Assign: AssignExtractor

  /** An extractor class to create and pattern match with syntax `Assign(lhs, rhs)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    lhs = rhs
   *  @group Extractors
   */
  abstract class AssignExtractor {
    def apply(lhs: Tree, rhs: Tree): Assign
    def unapply(assign: Assign): Option[(Tree, Tree)]
  }

  /** The API that all assigns support
   *  @group API
   */
  trait AssignApi extends TermTreeApi { this: Assign =>
    /** The left-hand side of the assignment.
     */
    def lhs: Tree

    /** The right-hand side of the assignment.
     */
    def rhs: Tree
  }

  /** Either an assignment or a named argument. Only appears in argument lists,
   *  eliminated by compiler phase typecheck (doTypedApply), resurrected by reifier.
   *  @group Trees
   *  @template
   */
  type NamedArg >: Null <: NamedArgApi with TermTree

  /** The constructor/extractor for `NamedArg` instances.
   *  @group Extractors
   */
  val NamedArg: NamedArgExtractor

  /** An extractor class to create and pattern match with syntax `NamedArg(lhs, rhs)`.
   *  This AST node corresponds to the following Scala code:
   *
   *  {{{
   *    m.f(lhs = rhs)
   *  }}}
   *  {{{
   *    @annotation(lhs = rhs)
   *  }}}
   *
   *  @group Extractors
   */
  abstract class NamedArgExtractor {
    def apply(lhs: Tree, rhs: Tree): NamedArg
    def unapply(namedArg: NamedArg): Option[(Tree, Tree)]
  }

  /** The API that all assigns support
   *  @group API
   */
  trait NamedArgApi extends TermTreeApi { this: NamedArg =>
    /** The left-hand side of the expression.
     */
    def lhs: Tree

    /** The right-hand side of the expression.
     */
    def rhs: Tree
  }

  /** Conditional expression
   *  @group Trees
   *  @template
   */
  type If >: Null <: IfApi with TermTree

  /** The constructor/extractor for `If` instances.
   *  @group Extractors
   */
  val If: IfExtractor

  /** An extractor class to create and pattern match with syntax `If(cond, thenp, elsep)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    `if` (cond) thenp `else` elsep
   *
   *  If the alternative is not present, the `elsep` is set to `Literal(Constant(()))`.
   *  @group Extractors
   */
  abstract class IfExtractor {
    def apply(cond: Tree, thenp: Tree, elsep: Tree): If
    def unapply(if_ : If): Option[(Tree, Tree, Tree)]
  }

  /** The API that all ifs support
   *  @group API
   */
  trait IfApi extends TermTreeApi { this: If =>
    /** The condition of the if.
     */
    def cond: Tree

    /** The main branch of the if.
     */
    def thenp: Tree

    /** The alternative of the if.
     *  Is equal to `Literal(Constant(()))` if not specified.
     */
    def elsep: Tree
  }

  /** - Pattern matching expression  (before compiler phase explicitouter before 2.10 / patmat from 2.10)
   *  - Switch statements            (after compiler phase explicitouter before 2.10 / patmat from 2.10)
   *
   *  After compiler phase explicitouter before 2.10 / patmat from 2.10, cases will satisfy the following constraints:
   *
   *  - all guards are `EmptyTree`,
   *  - all patterns will be either `Literal(Constant(x:Int))`
   *    or `Alternative(lit|...|lit)`
   *  - except for an "otherwise" branch, which has pattern
   *    `Ident(nme.WILDCARD)`
   *  @group Trees
   *  @template
   */
  type Match >: Null <: MatchApi with TermTree

  /** The constructor/extractor for `Match` instances.
   *  @group Extractors
   */
  val Match: MatchExtractor

  /** An extractor class to create and pattern match with syntax `Match(selector, cases)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    selector `match` { cases }
   *
   * `Match` is also used in pattern matching assignments like `val (foo, bar) = baz`.
   *  @group Extractors
   */
  abstract class MatchExtractor {
    def apply(selector: Tree, cases: List[CaseDef]): Match
    def unapply(match_ : Match): Option[(Tree, List[CaseDef])]
  }

  /** The API that all matches support
   *  @group API
   */
  trait MatchApi extends TermTreeApi { this: Match =>
    /** The scrutinee of the pattern match. */
    def selector: Tree

    /** The arms of the pattern match. */
    def cases: List[CaseDef]
  }

  /** Return expression
   *  @group Trees
   *  @template
   */
  type Return >: Null <: ReturnApi with SymTree with TermTree

  /** The constructor/extractor for `Return` instances.
   *  @group Extractors
   */
  val Return: ReturnExtractor

  /** An extractor class to create and pattern match with syntax `Return(expr)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    `return` expr
   *
   *  The symbol of a Return node is the enclosing method.
   *  @group Extractors
   */
  abstract class ReturnExtractor {
    def apply(expr: Tree): Return
    def unapply(return_ : Return): Option[Tree]
  }

  /** The API that all returns support
   *  @group API
   */
  trait ReturnApi extends TermTreeApi { this: Return =>
    /** The returned expression. */
    def expr: Tree
  }

  /** Try catch node
   *  @group Trees
   *  @template
   */
  type Try >: Null <: TryApi with TermTree

  /** The constructor/extractor for `Try` instances.
   *  @group Extractors
   */
  val Try: TryExtractor

  /** An extractor class to create and pattern match with syntax `Try(block, catches, finalizer)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    `try` block `catch` { catches } `finally` finalizer
   *
   *  If the finalizer is not present, the `finalizer` is set to `EmptyTree`.
   *  @group Extractors
   */
  abstract class TryExtractor {
    def apply(block: Tree, catches: List[CaseDef], finalizer: Tree): Try
    def unapply(try_ : Try): Option[(Tree, List[CaseDef], Tree)]
  }

  /** The API that all tries support
   *  @group API
   */
  trait TryApi extends TermTreeApi { this: Try =>
    /** The protected block. */
    def block: Tree

    /** The `catch` pattern-matching clauses of the try. */
    def catches: List[CaseDef]

    /** The `finally` part of the try. */
    def finalizer: Tree
  }

  /** Throw expression
   *  @group Trees
   *  @template
   */
  type Throw >: Null <: ThrowApi with TermTree

  /** The constructor/extractor for `Throw` instances.
   *  @group Extractors
   */
  val Throw: ThrowExtractor

  /** An extractor class to create and pattern match with syntax `Throw(expr)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    `throw` expr
   *  @group Extractors
   */
  abstract class ThrowExtractor {
    def apply(expr: Tree): Throw
    def unapply(throw_ : Throw): Option[Tree]
  }

  /** The API that all tries support
   *  @group API
   */
  trait ThrowApi extends TermTreeApi { this: Throw =>
    /** The thrown expression. */
    def expr: Tree
  }

  /** Object instantiation
   *  @group Trees
   *  @template
   */
  type New >: Null <: NewApi with TermTree

  /** The constructor/extractor for `New` instances.
   *  @group Extractors
   */
  val New: NewExtractor

  /** An extractor class to create and pattern match with syntax `New(tpt)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    `new` T
   *
   *  This node always occurs in the following context:
   *
   *    (`new` tpt).<init>[targs](args)
   *
   *  For example, an AST representation of:
   *
   *    new Example[Int](2)(3)
   *
   *  is the following code:
   *
   *    Apply(
   *      Apply(
   *        TypeApply(
   *          Select(New(TypeTree(typeOf[Example])), nme.CONSTRUCTOR)
   *          TypeTree(typeOf[Int])),
   *        List(Literal(Constant(2)))),
   *      List(Literal(Constant(3))))
   *  @group Extractors
   */
  abstract class NewExtractor {
    def apply(tpt: Tree): New
    def unapply(new_ : New): Option[Tree]
  }

  /** The API that all news support
   *  @group API
   */
  trait NewApi extends TermTreeApi { this: New =>
    /** The tree that represents the type being instantiated.
     *  See the example for [[scala.reflect.api.Trees#NewExtractor]].
     */
    def tpt: Tree
  }

  /** Type annotation, eliminated by compiler phase cleanup
   *  @group Trees
   *  @template
   */
  type Typed >: Null <: TypedApi with TermTree

  /** The constructor/extractor for `Typed` instances.
   *  @group Extractors
   */
  val Typed: TypedExtractor

  /** An extractor class to create and pattern match with syntax `Typed(expr, tpt)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    expr: tpt
   *  @group Extractors
   */
  abstract class TypedExtractor {
    def apply(expr: Tree, tpt: Tree): Typed
    def unapply(typed: Typed): Option[(Tree, Tree)]
  }

  /** The API that all typeds support
   *  @group API
   */
  trait TypedApi extends TermTreeApi { this: Typed =>
    /** The expression being ascribed with the type. */
    def expr: Tree

    /** The type being ascribed to the expression. */
    def tpt: Tree
  }

  /** Common base class for Apply and TypeApply.
   *  @group Trees
   *  @template
   */
  type GenericApply >: Null <: GenericApplyApi with TermTree

  /** The API that all applies support
   *  @group API
   */
  trait GenericApplyApi extends TermTreeApi { this: GenericApply =>
    /** The target of the application. */
    def fun: Tree

    /** The arguments of the application. */
    def args: List[Tree]
  }

  /* @PP: All signs point toward it being a requirement that args.nonEmpty,
   *  but I can't find that explicitly stated anywhere.  Unless your last name
   *  is odersky, you should probably treat it as true.
   */
  /** Explicit type application.
   *  @group Trees
   *  @template
   */
  type TypeApply >: Null <: TypeApplyApi with GenericApply

  /** The constructor/extractor for `TypeApply` instances.
   *  @group Extractors
   */
  val TypeApply: TypeApplyExtractor

  /** An extractor class to create and pattern match with syntax `TypeApply(fun, args)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    fun[args]
   *
   *  Should only be used with `fun` nodes which are terms, i.e. which have `isTerm` returning `true`.
   *  Otherwise `AppliedTypeTree` should be used instead.
   *
   *    def foo[T] = ???
   *    foo[Int] // represented as TypeApply(Ident(<foo>), List(TypeTree(<Int>)))
   *
   *    List[Int] as in `val x: List[Int] = ???`
   *    // represented as AppliedTypeTree(Ident(<List>), List(TypeTree(<Int>)))
   *
   *  @group Extractors
   */
  abstract class TypeApplyExtractor {
    def apply(fun: Tree, args: List[Tree]): TypeApply
    def unapply(typeApply: TypeApply): Option[(Tree, List[Tree])]
  }

  /** The API that all type applies support
   *  @group API
   */
  trait TypeApplyApi extends GenericApplyApi { this: TypeApply =>
  }

  /** Value application
   *  @group Trees
   *  @template
   */
  type Apply >: Null <: ApplyApi with GenericApply

  /** The constructor/extractor for `Apply` instances.
   *  @group Extractors
   */
  val Apply: ApplyExtractor

  /** An extractor class to create and pattern match with syntax `Apply(fun, args)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    fun(args)
   *
   *  For instance:
   *
   *    fun[targs](args)
   *
   *  Is expressed as:
   *
   *    Apply(TypeApply(fun, targs), args)
   *  @group Extractors
   */
  abstract class ApplyExtractor {
    def apply(fun: Tree, args: List[Tree]): Apply
    def unapply(apply: Apply): Option[(Tree, List[Tree])]
  }

  /** The API that all applies support
   *  @group API
   */
  trait ApplyApi extends GenericApplyApi { this: Apply =>
  }

  /** Super reference, where `qual` is the corresponding `this` reference.
   *  A super reference `C.super[M]` is represented as `Super(This(C), M)`.
   *  @group Trees
   *  @template
   */
  type Super >: Null <: SuperApi with TermTree

  /** The constructor/extractor for `Super` instances.
   *  @group Extractors
   */
  val Super: SuperExtractor

  /** An extractor class to create and pattern match with syntax `Super(qual, mix)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    C.super[M]
   *
   *  Which is represented as:
   *
   *    Super(This(C), M)
   *
   *  If `mix` is empty, it is tpnme.EMPTY.
   *
   *  The symbol of a Super is the class _from_ which the super reference is made.
   *  For instance in C.super(...), it would be C.
   *  @group Extractors
   */
  abstract class SuperExtractor {
    def apply(qual: Tree, mix: TypeName): Super
    def unapply(super_ : Super): Option[(Tree, TypeName)]
  }

  /** The API that all supers support
   *  @group API
   */
  trait SuperApi extends TermTreeApi { this: Super =>
    /** The qualifier of the `super` expression.
     *  See the example for [[scala.reflect.api.Trees#SuperExtractor]].
     */
    def qual: Tree

    /** The selector of the `super` expression.
     *  See the example for [[scala.reflect.api.Trees#SuperExtractor]].
     */
    def mix: TypeName
  }

  /** Self reference
   *  @group Trees
   *  @template
   */
  type This >: Null <: ThisApi with TermTree with SymTree

  /** The constructor/extractor for `This` instances.
   *  @group Extractors
   */
  val This: ThisExtractor

  /** An extractor class to create and pattern match with syntax `This(qual)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    qual.this
   *
   *  The symbol of a This is the class to which the this refers.
   *  For instance in C.this, it would be C.
   *  @group Extractors
   */
  abstract class ThisExtractor {
    def apply(qual: TypeName): This
    def unapply(this_ : This): Option[TypeName]
  }

  /** The API that all thises support
   *  @group API
   */
  trait ThisApi extends TermTreeApi with SymTreeApi { this: This =>
    /** The qualifier of the `this` expression.
     *  For an unqualified `this` refers to the enclosing class.
     */
    def qual: TypeName
  }

  /** A member selection <qualifier> . <name>
   *  @group Trees
   *  @template
   */
  type Select >: Null <: SelectApi with RefTree

  /** The constructor/extractor for `Select` instances.
   *  @group Extractors
   */
  val Select: SelectExtractor

  /** An extractor class to create and pattern match with syntax `Select(qual, name)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    qualifier.selector
   *
   *  Should only be used with `qualifier` nodes which are terms, i.e. which have `isTerm` returning `true`.
   *  Otherwise `SelectFromTypeTree` should be used instead.
   *
   *    foo.Bar // represented as Select(Ident(<foo>), <Bar>)
   *    Foo#Bar // represented as SelectFromTypeTree(Ident(<Foo>), <Bar>)
   *  @group Extractors
   */
  abstract class SelectExtractor {
    def apply(qualifier: Tree, name: Name): Select
    def unapply(select: Select): Option[(Tree, Name)]
  }

  /** The API that all selects support
   *  @group API
   */
  trait SelectApi extends RefTreeApi { this: Select =>
    /** @inheritdoc */
    def qualifier: Tree

    /** @inheritdoc */
    def name: Name
  }

  /** A reference to identifier `name`.
   *  @group Trees
   *  @template
   */
  type Ident >: Null <: IdentApi with RefTree

  /** The constructor/extractor for `Ident` instances.
   *  @group Extractors
   */
  val Ident: IdentExtractor

  /** An extractor class to create and pattern match with syntax `Ident(qual, name)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    name
   *
   *  Type checker converts idents that refer to enclosing fields or methods to selects.
   *  For example, name ==> this.name
   *  @group Extractors
   */
  abstract class IdentExtractor {
    def apply(name: Name): Ident
    def unapply(ident: Ident): Option[Name]
  }

  /** The API that all idents support
   *  @group API
   */
  trait IdentApi extends RefTreeApi { this: Ident =>
    /** Was this ident created from a backquoted identifier? */
    def isBackquoted: Boolean

    /** @inheritdoc */
    def name: Name
  }

  /** Literal
   *  @group Trees
   *  @template
   */
  type Literal >: Null <: LiteralApi with TermTree

  /** The constructor/extractor for `Literal` instances.
   *  @group Extractors
   */
  val Literal: LiteralExtractor

  /** An extractor class to create and pattern match with syntax `Literal(value)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    value
   *  @group Extractors
   */
  abstract class LiteralExtractor {
    def apply(value: Constant): Literal
    def unapply(literal: Literal): Option[Constant]
  }

  /** The API that all literals support
   *  @group API
   */
  trait LiteralApi extends TermTreeApi { this: Literal =>
    /** The compile-time constant underlying the literal. */
    def value: Constant
  }

  /** A tree that has an annotation attached to it. Only used for annotated types and
   *  annotation ascriptions, annotations on definitions are stored in the Modifiers.
   *  Eliminated by typechecker (typedAnnotated), the annotations are then stored in
   *  an AnnotatedType.
   *  @group Trees
   *  @template
   */
  type Annotated >: Null <: AnnotatedApi with Tree

  /** The constructor/extractor for `Annotated` instances.
   *  @group Extractors
   */
  val Annotated: AnnotatedExtractor

  /** An extractor class to create and pattern match with syntax `Annotated(annot, arg)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    arg @annot    // for types
   *    arg: @annot   // for exprs
   *  @group Extractors
   */
  abstract class AnnotatedExtractor {
    def apply(annot: Tree, arg: Tree): Annotated
    def unapply(annotated: Annotated): Option[(Tree, Tree)]
  }

  /** The API that all annotateds support
   *  @group API
   */
  trait AnnotatedApi extends TreeApi { this: Annotated =>
    /** The annotation. */
    def annot: Tree

    /** The annotee. */
    def arg: Tree
  }

  /** Singleton type, eliminated by RefCheck
   *  @group Trees
   *  @template
   */
  type SingletonTypeTree >: Null <: SingletonTypeTreeApi with TypTree

  /** The constructor/extractor for `SingletonTypeTree` instances.
   *  @group Extractors
   */
  val SingletonTypeTree: SingletonTypeTreeExtractor

  /** An extractor class to create and pattern match with syntax `SingletonTypeTree(ref)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    ref.type
   *  @group Extractors
   */
  abstract class SingletonTypeTreeExtractor {
    def apply(ref: Tree): SingletonTypeTree
    def unapply(singletonTypeTree: SingletonTypeTree): Option[Tree]
  }

  /** The API that all singleton type trees support
   *  @group API
   */
  trait SingletonTypeTreeApi extends TypTreeApi { this: SingletonTypeTree =>
    /** The underlying reference. */
    def ref: Tree
  }

  /** Type selection <qualifier> # <name>, eliminated by RefCheck
   *  @group Trees
   *  @template
   */
  type SelectFromTypeTree >: Null <: SelectFromTypeTreeApi with TypTree with RefTree

  /** The constructor/extractor for `SelectFromTypeTree` instances.
   *  @group Extractors
   */
  val SelectFromTypeTree: SelectFromTypeTreeExtractor

  /** An extractor class to create and pattern match with syntax `SelectFromTypeTree(qualifier, name)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    qualifier # selector
   *
   *  Note: a path-dependent type p.T is expressed as p.type # T
   *
   *  Should only be used with `qualifier` nodes which are types, i.e. which have `isType` returning `true`.
   *  Otherwise `Select` should be used instead.
   *
   *    Foo#Bar // represented as SelectFromTypeTree(Ident(<Foo>), <Bar>)
   *    foo.Bar // represented as Select(Ident(<foo>), <Bar>)
   *  @group Extractors
   */
  abstract class SelectFromTypeTreeExtractor {
    def apply(qualifier: Tree, name: TypeName): SelectFromTypeTree
    def unapply(selectFromTypeTree: SelectFromTypeTree): Option[(Tree, TypeName)]
  }

  /** The API that all selects from type trees support
   *  @group API
   */
  trait SelectFromTypeTreeApi extends TypTreeApi with RefTreeApi { this: SelectFromTypeTree =>
    /** @inheritdoc */
    def qualifier: Tree

    /** @inheritdoc */
    def name: TypeName
  }

  /** Intersection type <parent1> with ... with <parentN> { <decls> }, eliminated by RefCheck
   *  @group Trees
   *  @template
   */
  type CompoundTypeTree >: Null <: CompoundTypeTreeApi with TypTree

  /** The constructor/extractor for `CompoundTypeTree` instances.
   *  @group Extractors
   */
  val CompoundTypeTree: CompoundTypeTreeExtractor

  /** An extractor class to create and pattern match with syntax `CompoundTypeTree(templ)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    parent1 with ... with parentN { refinement }
   *  @group Extractors
   */
  abstract class CompoundTypeTreeExtractor {
    def apply(templ: Template): CompoundTypeTree
    def unapply(compoundTypeTree: CompoundTypeTree): Option[Template]
  }

  /** The API that all compound type trees support
   *  @group API
   */
  trait CompoundTypeTreeApi extends TypTreeApi { this: CompoundTypeTree =>
    /** The template of the compound type - represents the parents, the optional self-type and the optional definitions. */
    def templ: Template
  }

  /** Applied type <tpt> [ <args> ], eliminated by RefCheck
   *  @group Trees
   *  @template
   */
  type AppliedTypeTree >: Null <: AppliedTypeTreeApi with TypTree

  /** The constructor/extractor for `AppliedTypeTree` instances.
   *  @group Extractors
   */
  val AppliedTypeTree: AppliedTypeTreeExtractor

  /** An extractor class to create and pattern match with syntax `AppliedTypeTree(tpt, args)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    tpt[args]
   *
   *  Should only be used with `tpt` nodes which are types, i.e. which have `isType` returning `true`.
   *  Otherwise `TypeApply` should be used instead.
   *
   *    List[Int] as in `val x: List[Int] = ???`
   *    // represented as AppliedTypeTree(Ident(<List>), List(TypeTree(<Int>)))
   *
   *    def foo[T] = ???
   *    foo[Int] // represented as TypeApply(Ident(<foo>), List(TypeTree(<Int>)))
   *  @group Extractors
   */
  abstract class AppliedTypeTreeExtractor {
    def apply(tpt: Tree, args: List[Tree]): AppliedTypeTree
    def unapply(appliedTypeTree: AppliedTypeTree): Option[(Tree, List[Tree])]
  }

  /** The API that all applied type trees support
   *  @group API
   */
  trait AppliedTypeTreeApi extends TypTreeApi { this: AppliedTypeTree =>
    /** The target of the application. */
    def tpt: Tree

    /** The arguments of the application. */
    def args: List[Tree]
  }

  /** Type bounds tree node
   *  @group Trees
   *  @template
   */
  type TypeBoundsTree >: Null <: TypeBoundsTreeApi with TypTree

  /** The constructor/extractor for `TypeBoundsTree` instances.
   *  @group Extractors
   */
  val TypeBoundsTree: TypeBoundsTreeExtractor

  /** An extractor class to create and pattern match with syntax `TypeBoundsTree(lo, hi)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    >: lo <: hi
   *  @group Extractors
   */
  abstract class TypeBoundsTreeExtractor {
    def apply(lo: Tree, hi: Tree): TypeBoundsTree
    def unapply(typeBoundsTree: TypeBoundsTree): Option[(Tree, Tree)]
  }

  /** The API that all type bound trees support
   *  @group API
   */
  trait TypeBoundsTreeApi extends TypTreeApi { this: TypeBoundsTree =>
    /** The lower bound.
     *  Is equal to `Ident(<scala.Nothing>)` if not specified explicitly.
     */
    def lo: Tree

    /** The upper bound.
     *  Is equal to `Ident(<scala.Any>)` if not specified explicitly.
     */
    def hi: Tree
  }

  /** Existential type tree node
   *  @group Trees
   *  @template
   */
  type ExistentialTypeTree >: Null <: ExistentialTypeTreeApi with TypTree

  /** The constructor/extractor for `ExistentialTypeTree` instances.
   *  @group Extractors
   */
  val ExistentialTypeTree: ExistentialTypeTreeExtractor

  /** An extractor class to create and pattern match with syntax `ExistentialTypeTree(tpt, whereClauses)`.
   *  This AST node corresponds to the following Scala code:
   *
   *    tpt forSome { whereClauses }
   *  @group Extractors
   */
  abstract class ExistentialTypeTreeExtractor {
    def apply(tpt: Tree, whereClauses: List[MemberDef]): ExistentialTypeTree
    def unapply(existentialTypeTree: ExistentialTypeTree): Option[(Tree, List[MemberDef])]
  }

  /** The API that all existential type trees support
   *  @group API
   */
  trait ExistentialTypeTreeApi extends TypTreeApi { this: ExistentialTypeTree =>
    /** The underlying type of the existential type. */
    def tpt: Tree

    /** The clauses of the definition of the existential type.
     *  Elements are one of the following:
     *    1) TypeDef with TypeBoundsTree right-hand side
     *    2) ValDef with empty right-hand side
     */
    def whereClauses: List[MemberDef]
  }

  /** A synthetic tree holding an arbitrary type.  Not to be confused with
   *  with TypTree, the trait for trees that are only used for type trees.
   *  TypeTree's are inserted in several places, but most notably in
   *  `RefCheck`, where the arbitrary type trees are all replaced by
   *  TypeTree's.
   *  @group Trees
   *  @template
   */
  type TypeTree >: Null <: TypeTreeApi with TypTree

  /** The constructor/extractor for `TypeTree` instances.
   *  @group Extractors
   */
  val TypeTree: TypeTreeExtractor

  /** An extractor class to create and pattern match with syntax `TypeTree()`.
   *  This AST node does not have direct correspondence to Scala code,
   *  and is emitted by everywhere when we want to wrap a `Type` in a `Tree`.
   *  @group Extractors
   */
  abstract class TypeTreeExtractor {
    def apply(): TypeTree
    def unapply(typeTree: TypeTree): Boolean
  }

  /** The API that all type trees support
   *  @group API
   */
  trait TypeTreeApi extends TypTreeApi { this: TypeTree =>
    /** The precursor of this tree.
     *  Is equal to `EmptyTree` if this type tree doesn't have precursors.
     */
    def original: Tree
  }

  /** An empty deferred value definition corresponding to:
   *    val _: _
   *  This is used as a placeholder in the `self` parameter Template if there is
   *  no definition of a self value of self type.
   *  @group Trees
   */
  val noSelfType: ValDef

  @deprecated("use `noSelfType` instead", "2.11.0")
  val emptyValDef: ValDef

  /** An empty superclass constructor call corresponding to:
   *    super.<init>()
   *  This is used as a placeholder in the primary constructor body in class templates
   *  to denote the insertion point of a call to superclass constructor after the typechecker
   *  figures out the superclass of a given template.
   *  @group Trees
   */
  val pendingSuperCall: Apply

// ---------------------- factories ----------------------------------------------

  /** A factory method for `Block` nodes.
   *  Flattens directly nested blocks.
   *  @group Factories
   */
  @deprecated("use q\"{..$stats}\" instead. Flatten directly nested blocks manually if needed", "2.10.1")
  def Block(stats: Tree*): Block

  /** A factory method for `CaseDef` nodes.
   *  @group Factories
   */
  @deprecated("use cq\"$pat => $body\" instead", "2.10.1")
  def CaseDef(pat: Tree, body: Tree): CaseDef

  /** A factory method for `Bind` nodes.
   *  @group Factories
   */
  @deprecated("use the canonical Bind constructor to create a bind and then initialize its symbol manually", "2.10.1")
  def Bind(sym: Symbol, body: Tree): Bind

  /** A factory method for `Try` nodes.
   *  @group Factories
   */
  @deprecated("convert cases into casedefs and use q\"try $body catch { case ..$newcases }\" instead", "2.10.1")
  def Try(body: Tree, cases: (Tree, Tree)*): Try

  /** A factory method for `Throw` nodes.
   *  @group Factories
   */
  @deprecated("use q\"throw new $tpe(..$args)\" instead", "2.10.1")
  def Throw(tpe: Type, args: Tree*): Throw

  /** Factory method for object creation `new tpt(args_1)...(args_n)`
   *  A `New(t, as)` is expanded to: `(new t).<init>(as)`
   *  @group Factories
   */
  @deprecated("use q\"new $tpt(...$argss)\" instead", "2.10.1")
  def New(tpt: Tree, argss: List[List[Tree]]): Tree

  /** 0-1 argument list new, based on a type.
   *  @group Factories
   */
  @deprecated("use q\"new $tpe(..$args)\" instead", "2.10.1")
  def New(tpe: Type, args: Tree*): Tree

  /** 0-1 argument list new, based on a symbol.
   *  @group Factories
   */
  @deprecated("use q\"new $"+"{sym.toType}(..$"+"args)\" instead", "2.10.1")
  def New(sym: Symbol, args: Tree*): Tree

  /** A factory method for `Apply` nodes.
   *  @group Factories
   */
  @deprecated("use q\"$sym(..$args)\" instead", "2.10.1")
  def Apply(sym: Symbol, args: Tree*): Tree

  /** 0-1 argument list new, based on a type tree.
   *  @group Factories
   */
  @deprecated("use q\"new $tpt(..$args)\" instead", "2.10.1")
  def ApplyConstructor(tpt: Tree, args: List[Tree]): Tree

  /** A factory method for `Super` nodes.
   *  @group Factories
   */
  @deprecated("use q\"$sym.super[$mix].x\".qualifier instead", "2.10.1")
  def Super(sym: Symbol, mix: TypeName): Tree

  /** A factory method for `This` nodes.
   *  @group Factories
   */
  def This(sym: Symbol): Tree

  /** A factory method for `Select` nodes.
   *  The string `name` argument is assumed to represent a [[scala.reflect.api.Names#TermName `TermName`]].
   *  @group Factories
   */
  @deprecated("use Select(tree, TermName(name)) instead", "2.10.1")
  def Select(qualifier: Tree, name: String): Select

  /** A factory method for `Select` nodes.
   *  @group Factories
   */
  def Select(qualifier: Tree, sym: Symbol): Select

  /** A factory method for `Ident` nodes.
   *  @group Factories
   */
  @deprecated("use Ident(TermName(name)) instead", "2.10.1")
  def Ident(name: String): Ident

  /** A factory method for `Ident` nodes.
   *  @group Factories
   */
  def Ident(sym: Symbol): Ident

  /** A factory method for `TypeTree` nodes.
   *  @group Factories
   */
  def TypeTree(tp: Type): TypeTree

// ---------------------- copying ------------------------------------------------

  /** The type of standard (lazy) tree copiers.
   *  @template
   *  @group Copying
   */
  type TreeCopier >: Null <: AnyRef with TreeCopierOps

  /** The standard (lazy) tree copier.
   *  @group Copying
   */
  val treeCopy: TreeCopier = newLazyTreeCopier

  /** Creates a strict tree copier.
   *  @group Copying
   */
  def newStrictTreeCopier: TreeCopier

  /** Creates a lazy tree copier.
   *  @group Copying
   */
  def newLazyTreeCopier: TreeCopier

  /** The API of a tree copier.
   *  @group API
   */
  abstract class TreeCopierOps {
    /** Creates a `ClassDef` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def ClassDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], impl: Template): ClassDef

    /** Creates a `PackageDef` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def PackageDef(tree: Tree, pid: RefTree, stats: List[Tree]): PackageDef

    /** Creates a `ModuleDef` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def ModuleDef(tree: Tree, mods: Modifiers, name: Name, impl: Template): ModuleDef

    /** Creates a `ValDef` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def ValDef(tree: Tree, mods: Modifiers, name: Name, tpt: Tree, rhs: Tree): ValDef

    /** Creates a `DefDef` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def DefDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree): DefDef

    /** Creates a `TypeDef` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def TypeDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], rhs: Tree): TypeDef

    /** Creates a `LabelDef` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def LabelDef(tree: Tree, name: Name, params: List[Ident], rhs: Tree): LabelDef

    /** Creates a `Import` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def Import(tree: Tree, expr: Tree, selectors: List[ImportSelector]): Import

    /** Creates a `Template` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def Template(tree: Tree, parents: List[Tree], self: ValDef, body: List[Tree]): Template

    /** Creates a `Block` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def Block(tree: Tree, stats: List[Tree], expr: Tree): Block

    /** Creates a `CaseDef` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def CaseDef(tree: Tree, pat: Tree, guard: Tree, body: Tree): CaseDef

    /** Creates a `Alternative` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def Alternative(tree: Tree, trees: List[Tree]): Alternative

    /** Creates a `Star` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def Star(tree: Tree, elem: Tree): Star

    /** Creates a `Bind` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def Bind(tree: Tree, name: Name, body: Tree): Bind

    /** Creates a `UnApply` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def UnApply(tree: Tree, fun: Tree, args: List[Tree]): UnApply

    /** Creates a `Function` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def Function(tree: Tree, vparams: List[ValDef], body: Tree): Function

    /** Creates a `Assign` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def Assign(tree: Tree, lhs: Tree, rhs: Tree): Assign

    /** Creates a `NamedArg` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def NamedArg(tree: Tree, lhs: Tree, rhs: Tree): NamedArg

    /** Creates a `If` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def If(tree: Tree, cond: Tree, thenp: Tree, elsep: Tree): If

    /** Creates a `Match` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def Match(tree: Tree, selector: Tree, cases: List[CaseDef]): Match

    /** Creates a `Return` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def Return(tree: Tree, expr: Tree): Return

    /** Creates a `Try` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def Try(tree: Tree, block: Tree, catches: List[CaseDef], finalizer: Tree): Try

    /** Creates a `Throw` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def Throw(tree: Tree, expr: Tree): Throw

    /** Creates a `New` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def New(tree: Tree, tpt: Tree): New

    /** Creates a `Typed` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def Typed(tree: Tree, expr: Tree, tpt: Tree): Typed

    /** Creates a `TypeApply` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def TypeApply(tree: Tree, fun: Tree, args: List[Tree]): TypeApply

    /** Creates a `Apply` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def Apply(tree: Tree, fun: Tree, args: List[Tree]): Apply

    /** Creates a `Super` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def Super(tree: Tree, qual: Tree, mix: TypeName): Super

    /** Creates a `This` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def This(tree: Tree, qual: Name): This

    /** Creates a `Select` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def Select(tree: Tree, qualifier: Tree, selector: Name): Select

    /** Creates a `Ident` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def Ident(tree: Tree, name: Name): Ident

    /** Creates a `RefTree` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def RefTree(tree: Tree, qualifier: Tree, selector: Name): RefTree

    /** Creates a `ReferenceToBoxed` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def ReferenceToBoxed(tree: Tree, idt: Ident): ReferenceToBoxed

    /** Creates a `Literal` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def Literal(tree: Tree, value: Constant): Literal

    /** Creates a `TypeTree` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def TypeTree(tree: Tree): TypeTree

    /** Creates a `Annotated` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def Annotated(tree: Tree, annot: Tree, arg: Tree): Annotated

    /** Creates a `SingletonTypeTree` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def SingletonTypeTree(tree: Tree, ref: Tree): SingletonTypeTree

    /** Creates a `SelectFromTypeTree` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def SelectFromTypeTree(tree: Tree, qualifier: Tree, selector: Name): SelectFromTypeTree

    /** Creates a `CompoundTypeTree` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def CompoundTypeTree(tree: Tree, templ: Template): CompoundTypeTree

    /** Creates a `AppliedTypeTree` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def AppliedTypeTree(tree: Tree, tpt: Tree, args: List[Tree]): AppliedTypeTree

    /** Creates a `TypeBoundsTree` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def TypeBoundsTree(tree: Tree, lo: Tree, hi: Tree): TypeBoundsTree

    /** Creates a `ExistentialTypeTree` node from the given components, having a given `tree` as a prototype.
     *  Having a tree as a prototype means that the tree's attachments, type and symbol will be copied into the result.
     */
    def ExistentialTypeTree(tree: Tree, tpt: Tree, whereClauses: List[MemberDef]): ExistentialTypeTree
  }

// ---------------------- traversing and transforming ------------------------------

  /** A class that implement a default tree traversal strategy: breadth-first component-wise.
   *  @group Traversal
   */
  class Traverser {
    protected[scala] var currentOwner: Symbol = rootMirror.RootClass

    /** Traverse something which Trees contain, but which isn't a Tree itself. */
    def traverseName(name: Name): Unit                    = ()
    def traverseConstant(c: Constant): Unit               = ()
    def traverseImportSelector(sel: ImportSelector): Unit = ()
    def traverseModifiers(mods: Modifiers): Unit          = traverseAnnotations(mods.annotations)

    /** Traverses a single tree. */
    def traverse(tree: Tree): Unit              = itraverse(this, tree): @nowarn("cat=deprecation")
    def traversePattern(pat: Tree): Unit        = traverse(pat)
    def traverseGuard(guard: Tree): Unit        = traverse(guard)
    def traverseTypeAscription(tpt: Tree): Unit = traverse(tpt)
    // Special handling of noSelfType necessary for backward compat: existing
    // traversers break down when they see the unexpected tree.
    def traverseSelfType(self: ValDef): Unit    = if (self ne noSelfType) traverse(self)

    /** Traverses a list of trees. */
    def traverseTrees(trees: List[Tree]): Unit          = trees foreach traverse
    def traverseTypeArgs(args: List[Tree]): Unit        = traverseTrees(args)
    def traverseParents(parents: List[Tree]): Unit      = traverseTrees(parents)
    def traverseCases(cases: List[CaseDef]): Unit       = traverseTrees(cases)
    def traverseAnnotations(annots: List[Tree]): Unit   = traverseTrees(annots)

    /** Traverses a list of lists of trees. */
    def traverseTreess(treess: List[List[Tree]]): Unit    = treess foreach traverseTrees
    def traverseParams(params: List[Tree]): Unit          = traverseTrees(params)
    def traverseParamss(vparamss: List[List[Tree]]): Unit = vparamss foreach traverseParams

    /** Traverses a list of trees with a given owner symbol. */
    def traverseStats(stats: List[Tree], exprOwner: Symbol): Unit = {
      stats foreach (stat =>
        if (exprOwner != currentOwner) atOwner(exprOwner)(traverse(stat))
        else traverse(stat)
      )
    }

    /** Performs a traversal with a given owner symbol. */
    def atOwner(owner: Symbol)(traverse: => Unit): Unit = {
      val prevOwner = currentOwner
      currentOwner = owner
      traverse
      currentOwner = prevOwner
    }

    /** Leave apply available in the generic traverser to do something else.
     */
    def apply[T <: Tree](tree: T): T = { traverse(tree); tree }
  }

  /** Delegates the traversal strategy to `scala.reflect.internal.Trees`,
   *  because pattern matching on abstract types we have here degrades performance.
   *  @group Traversal
   */
  // FIXME: `Tree`/`TreeApi` does not contain a `traverse` method, so methods
  //        calling this (and not its override) are unable to follow the deprecation
  //        message. Once this is fixed, please fix callers of this method and remove
  //        the `@nowarn` annotation from them
  @deprecated("Use Tree#traverse instead", "2.12.3")
  protected def itraverse(traverser: Traverser, tree: Tree): Unit = throw new MatchError(tree)

  /** Provides an extension hook for the traversal strategy.
   *  Future-proofs against new node types.
   *  @group Traversal
   */
  // FIXME: `Tree`/`TreeApi` does not contain a `traverse` method, so methods
  //        calling this (and not its override) are unable to follow the deprecation
  //        message. Once this is fixed, please fix callers of this method and remove
  //        the `@nowarn` annotation from them
  @deprecated("Use Tree#traverse instead", "2.12.3")
  protected def xtraverse(traverser: Traverser, tree: Tree): Unit = throw new MatchError(tree)

  /** A class that implement a default tree transformation strategy: breadth-first component-wise cloning.
   *  @group Traversal
   */
  abstract class Transformer {
    /** The underlying tree copier. */
    val treeCopy: TreeCopier = newLazyTreeCopier

    /** The current owner symbol. */
    protected[scala] var currentOwner: Symbol = rootMirror.RootClass

    /** The enclosing method of the currently transformed tree. */
    protected def currentMethod = {
      @tailrec def enclosingMethod(sym: Symbol): Symbol =
        if (sym.isMethod || sym == NoSymbol) sym else enclosingMethod(sym.owner)
      enclosingMethod(currentOwner)
    }

    /** The enclosing class of the currently transformed tree. */
    protected def currentClass = {
      @tailrec def enclosingClass(sym: Symbol): Symbol =
        if (sym.isClass || sym == NoSymbol) sym else enclosingClass(sym.owner)
      enclosingClass(currentOwner)
    }

//    protected def currentPackage = currentOwner.enclosingTopLevelClass.owner

    /** Transforms a single tree. */
    def transform(tree: Tree): Tree = itransform(this, tree): @nowarn("cat=deprecation")

    /** Transforms a list of trees. */
    def transformTrees(trees: List[Tree]): List[Tree] =
      if (trees.isEmpty) Nil else trees mapConserve transform

    /** Transforms a `Template`. */
    def transformTemplate(tree: Template): Template =
      transform(tree: Tree).asInstanceOf[Template]
    /** Transforms a list of `TypeDef` trees. */
    def transformTypeDefs(trees: List[TypeDef]): List[TypeDef] =
      trees mapConserve (tree => transform(tree).asInstanceOf[TypeDef])
    /** Transforms a `ValDef`. */
    def transformValDef(tree: ValDef): ValDef =
      if (tree eq noSelfType) tree
      else transform(tree).asInstanceOf[ValDef]
    /** Transforms a list of `ValDef` nodes. */
    def transformValDefs(trees: List[ValDef]): List[ValDef] =
      trees mapConserve (transformValDef(_))
    /** Transforms a list of lists of `ValDef` nodes. */
    def transformValDefss(treess: List[List[ValDef]]): List[List[ValDef]] =
      treess mapConserve (transformValDefs(_))
    /** Transforms a list of `MemberDef` nodes. */
    def transformMemberDefs(trees: List[MemberDef]): List[MemberDef] =
      trees mapConserve (tree => transform(tree).asInstanceOf[MemberDef])
    /** Transforms a list of `CaseDef` nodes. */
    def transformCaseDefs(trees: List[CaseDef]): List[CaseDef] =
      trees mapConserve (tree => transform(tree).asInstanceOf[CaseDef])
    /** Transforms a list of `Ident` nodes. */
    def transformIdents(trees: List[Ident]): List[Ident] =
      trees mapConserve (tree => transform(tree).asInstanceOf[Ident])
    /** Traverses a list of trees with a given owner symbol. */
    def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] =
      stats mapConserve (stat =>
        if (exprOwner != currentOwner && stat.isTerm) atOwner(exprOwner)(transform(stat))
        else transform(stat)) filter (EmptyTree != _)
    /** Transforms `Modifiers`. */
    def transformModifiers(mods: Modifiers): Modifiers = {
      if (mods.annotations.isEmpty) mods
      else mods mapAnnotations transformTrees
    }

    /** Transforms a tree with a given owner symbol. */
    def atOwner[A](owner: Symbol)(trans: => A): A = {
      val prevOwner = currentOwner
      currentOwner = owner
      val result = trans
      currentOwner = prevOwner
      result
    }
  }

  /** Delegates the transformation strategy to `scala.reflect.internal.Trees`,
   *  because pattern matching on abstract types we have here degrades performance.
   *  @group Traversal
   */
  // FIXME: `Tree`/`TreeApi` does not contain a `transform` method, so methods
  //        calling this (and not its override) are unable to follow the deprecation
  //        message. Once this is fixed, please fix callers of this method and remove
  //        the `@nowarn` annotation from them
  @deprecated("Use Tree#transform instead", since = "2.13.4")
  protected def itransform(transformer: Transformer, tree: Tree): Tree = throw new MatchError(tree)

  /** Provides an extension hook for the transformation strategy.
   *  Future-proofs against new node types.
   *  @group Traversal
   */
  protected def xtransform(transformer: Transformer, tree: Tree): Tree = throw new MatchError(tree)

  /** The type of tree modifiers (not a tree, but rather part of DefTrees).
   *  @group Traversal
   */
  type Modifiers >: Null <: AnyRef with ModifiersApi

  /** The API that all Modifiers support
   *  @group API
   */
  abstract class ModifiersApi {
    /** The underlying flags of the enclosing definition.
     *  Is equal to `NoFlags` if none are specified explicitly.
     */
    def flags: FlagSet

    def hasFlag(flag: FlagSet): Boolean

    /** The visibility scope of the enclosing definition.
     *  Is equal to `tpnme.EMPTY` if none is specified explicitly.
     */
    def privateWithin: Name

    /** The annotations of the enclosing definition.
     *  Empty list if none are specified explicitly.
     */
    def annotations: List[Tree]

    /** Creates a new instance of `Modifiers` with
     *  the annotations transformed according to the given function.
     */
    def mapAnnotations(f: List[Tree] => List[Tree]): Modifiers =
      Modifiers(flags, privateWithin, f(annotations))
  }

  /** The constructor/extractor for `Modifiers` instances.
   *  @group Traversal
   */
  val Modifiers: ModifiersExtractor

  @deprecated("use ModifiersExtractor instead", "2.11.0")
  type ModifiersCreator = ModifiersExtractor

  /** An extractor class to create and pattern match with syntax `Modifiers(flags, privateWithin, annotations)`.
   *  Modifiers encapsulate flags, visibility annotations and Scala annotations for member definitions.
   *  @group Traversal
   */
  abstract class ModifiersExtractor {
    def apply(): Modifiers = Modifiers(NoFlags, typeNames.EMPTY, List())
    def apply(flags: FlagSet, privateWithin: Name, annotations: List[Tree]): Modifiers
    def unapply(mods: Modifiers): Option[(FlagSet, Name, List[Tree])]
  }

  /** The factory for `Modifiers` instances.
   *  @group Traversal
   */
  def Modifiers(flags: FlagSet, privateWithin: Name): Modifiers = Modifiers(flags, privateWithin, List())

  /** The factory for `Modifiers` instances.
   *  @group Traversal
   */
  def Modifiers(flags: FlagSet): Modifiers = Modifiers(flags, typeNames.EMPTY)

  /** An empty `Modifiers` object: no flags, empty visibility annotation and no Scala annotations.
   *  @group Traversal
   */
  lazy val NoMods = Modifiers()
}
