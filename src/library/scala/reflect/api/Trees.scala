/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package api

import scala.collection.mutable.ListBuffer

// Syncnote: Trees are currently not thread-safe.
trait Trees { self: Universe =>

  private[scala] var nodeCount = 0

  type Modifiers >: Null <: AbsModifiers
  val NoMods: Modifiers

  // TODO - Where do I put this?
  object BackquotedIdentifier

  abstract class AbsModifiers {
    def modifiers: Set[Modifier]
    def hasModifier(mod: Modifier): Boolean
    def privateWithin: Name  // default: EmptyTypeName
    def annotations: List[Tree] // default: List()
    def mapAnnotations(f: List[Tree] => List[Tree]): Modifiers
  }

  def Modifiers(mods: Set[Modifier] = Set(),
                privateWithin: Name = EmptyTypeName,
                annotations: List[Tree] = List()): Modifiers

  /** Tree is the basis for scala's abstract syntax. The nodes are
   *  implemented as case classes, and the parameters which initialize
   *  a given tree are immutable: however Trees have several mutable
   *  fields which are manipulated in the course of typechecking,
   *  including pos, symbol, and tpe.
   *
   *  Newly instantiated trees have tpe set to null (though it
   *  may be set immediately thereafter depending on how it is
   *  constructed.) When a tree is passed to the typer, typically via
   *  `typer.typed(tree)`, under normal circumstances the tpe must be
   *  null or the typer will ignore it. Furthermore, the typer is not
   *  required to return the same tree it was passed.
   *
   *  Trees can be easily traversed with e.g. foreach on the root node;
   *  for a more nuanced traversal, subclass Traverser. Transformations
   *  can be considerably trickier: see the numerous subclasses of
   *  Transformer found around the compiler.
   *
   *  Copying Trees should be done with care depending on whether
   *  it need be done lazily or strictly (see LazyTreeCopier and
   *  StrictTreeCopier) and on whether the contents of the mutable
   *  fields should be copied. The tree copiers will copy the mutable
   *  attributes to the new tree; calling Tree#duplicate will copy
   *  symbol and tpe, but all the positions will be focused.
   *
   *  Trees can be coarsely divided into four mutually exclusive categories:
   *
   *  - TermTrees, representing terms
   *  - TypTrees, representing types.  Note that is `TypTree`, not `TypeTree`.
   *  - SymTrees, which may represent types or terms.
   *  - Other Trees, which have none of those as parents.
   *
   *  SymTrees include important nodes Ident and Select, which are
   *  used as both terms and types; they are distinguishable based on
   *  whether the Name is a TermName or TypeName.  The correct way for
   *  to test for a type or a term (on any Tree) are the isTerm/isType
   *  methods on Tree.
   *
   *  "Others" are mostly syntactic or short-lived constructs. Examples
   *  include CaseDef, which wraps individual match cases: they are
   *  neither terms nor types, nor do they carry a symbol. Another
   *  example is Parens, which is eliminated during parsing.
   */
  abstract class Tree extends Product {
    val id = nodeCount
    nodeCount += 1

    /** Prefix under which to print this tree type.  Defaults to product
     *  prefix (e.g. DefTree) but because that is used in reification
     *  it cannot be altered without breaking reflection.
     */
    def printingPrefix = productPrefix

    def pos: Position = rawatt.pos.asInstanceOf[Position] // [Eugene] how do we get rid of this cast?
    def pos_=(pos: Position): Unit = rawatt = (rawatt withPos pos) // the "withPos" part is crucial to robustness
    def setPos(newpos: Position): this.type = { pos = newpos; this }

    // [Eugene] can we make this more type-safe
    private var rawatt: Attachment = NoPosition
    def attach(att: Any): Unit =
      rawatt match {
        case NontrivialAttachment(pos, payload) =>
          val index = payload.indexWhere(p => p.getClass == att.getClass)
          if (index == -1) payload += att
          else payload(index) = att
        case _ =>
          rawatt = NontrivialAttachment(pos, collection.mutable.ListBuffer[Any](att))
      }

    // a) why didn't this method already exist
    // b) what is all this "Any" business?
    // c) am I reverse-engineering this correctly? It shouldn't be hard
    //    to figure out what is attached.
    def attachments: List[Any] = rawatt match {
      case NoPosition                      => Nil
      case NontrivialAttachment(pos, atts) => pos :: atts.toList
      case x                               => List(x)
    }
    // Writing "Any" repeatedly to work within this structure
    // is making my skin crawl.
    def hasAttachment(x: Any) = attachments contains x

    def withAttachment(att: Any): this.type = { attach(att); this }
    def detach(att: Any): Unit =
      detach(att.getClass)
    def detach(clazz: java.lang.Class[_]): Unit =
      rawatt match {
        case NontrivialAttachment(pos, payload) =>
          val index = payload.indexWhere(p => p.getClass == clazz)
          if (index != -1) payload.remove(index)
        case _ =>
          // do nothing
      }
    def withoutAttachment(att: Any): this.type = { detach(att); this }
    def attachment[T: ClassTag]: T = attachmentOpt[T] getOrElse { throw new Error("no attachment of type %s".format(classTag[T].erasure)) }
    def attachmentOpt[T: ClassTag]: Option[T] =
      firstAttachment { case attachment if attachment.getClass == classTag[T].erasure => attachment.asInstanceOf[T] }

    def firstAttachment[T](p: PartialFunction[Any, T]): Option[T] =
      rawatt match {
        case NontrivialAttachment(pos, payload) => payload.collectFirst(p)
        case _ => None
      }

    private[this] var rawtpe: Type = _

    def tpe = rawtpe
    def tpe_=(t: Type) = rawtpe = t

    def resetType(): this.type   = { tpe = null ; this }
    def resetSymbol(): this.type = { if (hasSymbol) symbol = NoSymbol ; this }

    /** Set tpe to give `tp` and return this.
     */
    def setType(tp: Type): this.type = { rawtpe = tp; this }

    /** Like `setType`, but if this is a previously empty TypeTree that
     *  fact is remembered so that resetAllAttrs will snap back.
     *
     *  @PP: Attempting to elaborate on the above, I find: If defineType
     *  is called on a TypeTree whose type field is null or NoType,
     *  this is recorded as "wasEmpty = true". That value is used in
     *  ResetAttrs, which nulls out the type field of TypeTrees
     *  for which wasEmpty is true, leaving the others alone.
     *
     *  resetAllAttrs is used in situations where some speculative
     *  typing of a tree takes place, fails, and the tree needs to be
     *  returned to its former state to try again. So according to me:
     *  using `defineType` instead of `setType` is how you communicate
     *  that the type being set does not depend on any previous state,
     *  and therefore should be abandoned if the current line of type
     *  inquiry doesn't work out.
     */
    def defineType(tp: Type): this.type = setType(tp)

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
    def symbol: Symbol = null
    def symbol_=(sym: Symbol) { throw new UnsupportedOperationException("symbol_= inapplicable for " + this) }
    def setSymbol(sym: Symbol): this.type = { symbol = sym; this }

    def hasSymbol = false
    def isDef = false
    def isEmpty = false
    @inline final def orElse(alt: => Tree) = if (!isEmpty) this else alt
    @inline final def andAlso(f: Tree => Unit): Tree = { if (!this.isEmpty) f(this) ; this }

    def hasAssignedType   = (tpe ne null) && (tpe ne NoType)
    def hasAssignedSymbol = (symbol ne null) && (symbol ne NoSymbol)

    @inline final def hasSymbolWhich(f: Symbol => Boolean) = hasAssignedSymbol && f(symbol)
    @inline final def hasTypeWhich(f: Type => Boolean)     = hasAssignedType && f(tpe)

    /** The canonical way to test if a Tree represents a term.
     */
    def isTerm: Boolean = this match {
      case _: TermTree       => true
      case Bind(name, _)     => name.isTermName
      case Select(_, name)   => name.isTermName
      case Ident(name)       => name.isTermName
      case Annotated(_, arg) => arg.isTerm
      case _                 => false
    }

    /** The canonical way to test if a Tree represents a type.
     */
    def isType: Boolean = this match {
      case _: TypTree        => true
      case Bind(name, _)     => name.isTypeName
      case Select(_, name)   => name.isTypeName
      case Ident(name)       => name.isTypeName
      case Annotated(_, arg) => arg.isType
      case _                 => false
    }

    /** Apply `f` to each subtree */
    def foreach(f: Tree => Unit) { new ForeachTreeTraverser(f).traverse(this) }

    /** Find all subtrees matching predicate `p` */
    def withFilter(f: Tree => Boolean): List[Tree] = {
      val ft = new FilterTreeTraverser(f)
      ft.traverse(this)
      ft.hits.toList
    }
    def filter(f: Tree => Boolean): List[Tree] = withFilter(f)

    /** Apply `pf' to each subtree on which the function is defined */
    def collect[T](pf: PartialFunction[Tree, T]): List[T] = {
      val ctt = new CollectTreeTraverser[T](pf)
      ctt.traverse(this)
      ctt.results.toList
    }

    /** Returns optionally first tree (in a preorder traversal) which satisfies predicate `p`,
     *  or None if none exists.
     */
    def find(p: Tree => Boolean): Option[Tree] = {
      val ft = new FindTreeTraverser(p)
      ft.traverse(this)
      ft.result
    }

    /** Is there exists a part of this tree which satisfies predicate `p`? */
    def exists(p: Tree => Boolean): Boolean = !find(p).isEmpty

    /** Do all parts of this tree satisfy predicate `p`? */
    def forAll(p: Tree => Boolean): Boolean = find(!p(_)).isEmpty

    def equalsStructure(that : Tree) = equalsStructure0(that)(_ eq _)
    def equalsStructure0(that: Tree)(f: (Tree,Tree) => Boolean): Boolean =
      f(this, that) || ((this.productArity == that.productArity) && {
        def equals0(this0: Any, that0: Any): Boolean = (this0, that0) match {
          case (x: Tree, y: Tree)         => f(x, y) || (x equalsStructure0 y)(f)
          case (xs: List[_], ys: List[_]) => (xs corresponds ys)(equals0)
          case _                          => this0 == that0
        }
        def compareOriginals() = (this, that) match {
          case (x: TypeTree, y: TypeTree) if x.original != null && y.original != null =>
            (x.original equalsStructure0 y.original)(f)
          case _                          =>
            true
        }

        (this.productIterator zip that.productIterator forall { case (x, y) => equals0(x, y) }) && compareOriginals()
      })

    /** The direct child trees of this tree.
     *  EmptyTrees are always omitted.  Lists are flattened.
     */
    def children: List[Tree] = {
      def subtrees(x: Any): List[Tree] = x match {
        case EmptyTree   => Nil
        case t: Tree     => List(t)
        case xs: List[_] => xs flatMap subtrees
        case _           => Nil
      }
      productIterator.toList flatMap subtrees
    }

    /** Make a copy of this tree, keeping all attributes,
     *  except that all positions are focused (so nothing
     *  in this tree will be found when searching by position).
     */
    def duplicate: this.type =
      duplicateTree(this).asInstanceOf[this.type]

    private[scala] def copyAttrs(tree: Tree): this.type = {
      rawatt = tree.rawatt
      tpe = tree.tpe
      if (hasSymbol) symbol = tree.symbol
      this
    }

    override def toString: String = show(this)
    override def hashCode(): Int = System.identityHashCode(this)
    override def equals(that: Any) = this eq that.asInstanceOf[AnyRef]
  }

  /** A tree for a term.  Not all terms are TermTrees; use isTerm
   *  to reliably identify terms.
   */
  trait TermTree extends Tree

  /** A tree for a type.  Not all types are TypTrees; use isType
   *  to reliably identify types.
   */
  trait TypTree extends Tree

  /** A tree with a mutable symbol field, initialized to NoSymbol.
   */
  trait SymTree extends Tree {
    override def hasSymbol = true
    override var symbol: Symbol = NoSymbol
  }

  /** A tree with a name - effectively, a DefTree or RefTree.
   */
  trait NameTree extends Tree {
    def name: Name
  }

  /** A tree which references a symbol-carrying entity.
   *  References one, as opposed to defining one; definitions
   *  are in DefTrees.
   */
  trait RefTree extends SymTree with NameTree {
    def qualifier: Tree    // empty for Idents
    def name: Name
  }

  /** A tree which defines a symbol-carrying entity.
   */
  abstract class DefTree extends SymTree with NameTree {
    def name: Name
    override def isDef = true
  }

// ----- tree node alternatives --------------------------------------

  /** The empty tree */
  case object EmptyTree extends TermTree {
    super.tpe_=(NoType)
    override def tpe_=(t: Type) =
      if (t != NoType) throw new UnsupportedOperationException("tpe_=("+t+") inapplicable for <empty>")
    override def isEmpty = true
    override def resetType(): this.type = this
  }

  /** Common base class for all member definitions: types, classes,
   *  objects, packages, vals and vars, defs.
   */
  abstract class MemberDef extends DefTree {
    def mods: Modifiers
    def keyword: String = this match {
      case TypeDef(_, _, _, _)      => "type"
      case ClassDef(mods, _, _, _)  => if (mods hasModifier Modifier.`trait`) "trait" else "class"
      case DefDef(_, _, _, _, _, _) => "def"
      case ModuleDef(_, _, _)       => "object"
      case PackageDef(_, _)         => "package"
      case ValDef(mods, _, _, _)    => if (mods hasModifier Modifier.mutable) "var" else "val"
      case _ => ""
    }
  }

  /** A packaging, such as `package pid { stats }`
   */
  case class PackageDef(pid: RefTree, stats: List[Tree])
       extends MemberDef {
    def name = pid.name
    def mods = Modifiers()
  }

  /** A common base class for class and object definitions.
   */
  abstract class ImplDef extends MemberDef {
    def impl: Template
  }

  /** A class definition.
   */
  case class ClassDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], impl: Template)
       extends ImplDef

  /** @param sym       the class symbol
   *  @return          the implementation template
   */
  def ClassDef(sym: Symbol, impl: Template): ClassDef

  /** An object definition, e.g. `object Foo`.  Internally, objects are
   *  quite frequently called modules to reduce ambiguity.
   */
  case class ModuleDef(mods: Modifiers, name: TermName, impl: Template)
        extends ImplDef

  /**
   *  @param sym       the class symbol
   *  @param impl      the implementation template
   */
  def ModuleDef(sym: Symbol, impl: Template): ModuleDef

  /** A common base class for ValDefs and DefDefs.
   */
  abstract class ValOrDefDef extends MemberDef {
    def name: Name // can't be a TermName because macros can be type names.
    def tpt: Tree
    def rhs: Tree
  }

  /** Broadly speaking, a value definition.  All these are encoded as ValDefs:
   *
   *   - immutable values, e.g. "val x"
   *   - mutable values, e.g. "var x" - the MUTABLE flag set in mods
   *   - lazy values, e.g. "lazy val x" - the LAZY flag set in mods
   *   - method parameters, see vparamss in DefDef - the PARAM flag is set in mods
   *   - explicit self-types, e.g. class A { self: Bar => } - !!! not sure what is set.
   */
  case class ValDef(mods: Modifiers, name: TermName, tpt: Tree, rhs: Tree) extends ValOrDefDef

  def ValDef(sym: Symbol, rhs: Tree): ValDef

  def ValDef(sym: Symbol): ValDef

  /** A method or macro definition.
   *  @param name   The name of the method or macro. Can be a type name in case this is a type macro
   */
  case class DefDef(mods: Modifiers, name: Name, tparams: List[TypeDef],
                    vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree) extends ValOrDefDef

  def DefDef(sym: Symbol, mods: Modifiers, vparamss: List[List[ValDef]], rhs: Tree): DefDef

  def DefDef(sym: Symbol, vparamss: List[List[ValDef]], rhs: Tree): DefDef

  def DefDef(sym: Symbol, mods: Modifiers, rhs: Tree): DefDef

  def DefDef(sym: Symbol, rhs: Tree): DefDef

  def DefDef(sym: Symbol, rhs: List[List[Symbol]] => Tree): DefDef

  /** An abstract type, a type parameter, or a type alias.
   */
  case class TypeDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], rhs: Tree)
       extends MemberDef

  /** A TypeDef node which defines given `sym` with given tight hand side `rhs`. */
  def TypeDef(sym: Symbol, rhs: Tree): TypeDef

  /** A TypeDef node which defines abstract type or type parameter for given `sym` */
  def TypeDef(sym: Symbol): TypeDef

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
   */
  case class LabelDef(name: TermName, params: List[Ident], rhs: Tree)
       extends DefTree with TermTree

  def LabelDef(sym: Symbol, params: List[Symbol], rhs: Tree): LabelDef

  /** Import selector
   *
   * Representation of an imported name its optional rename and their optional positions
   *
   * @param name      the imported name
   * @param namePos   its position or -1 if undefined
   * @param rename    the name the import is renamed to (== name if no renaming)
   * @param renamePos the position of the rename or -1 if undefined
   */
  case class ImportSelector(name: Name, namePos: Int, rename: Name, renamePos: Int)

  /** Import clause
   *
   *  @param expr
   *  @param selectors
   */
  case class Import(expr: Tree, selectors: List[ImportSelector])
       extends SymTree
    // The symbol of an Import is an import symbol @see Symbol.newImport
    // It's used primarily as a marker to check that the import has been typechecked.

  /** Instantiation template of a class or trait
   *
   *  @param parents
   *  @param body
   */
  case class Template(parents: List[Tree], self: ValDef, body: List[Tree])
       extends SymTree {
    // the symbol of a template is a local dummy. @see Symbol.newLocalDummy
    // the owner of the local dummy is the enclosing trait or class.
    // the local dummy is itself the owner of any local blocks
    // For example:
    //
    // class C {
    //   def foo // owner is C
    //   {
    //      def bar  // owner is local dummy
    //   }
  }

  /** Block of expressions (semicolon separated expressions) */
  case class Block(stats: List[Tree], expr: Tree)
       extends TermTree

  /** Block factory that flattens directly nested blocks.
   */
  def Block(stats: Tree*): Block

  /** Case clause in a pattern match, eliminated during explicitouter
   *  (except for occurrences in switch statements)
   */
  case class CaseDef(pat: Tree, guard: Tree, body: Tree)
       extends Tree

  /** casedef shorthand */
  def CaseDef(pat: Tree, body: Tree): CaseDef

  /** Alternatives of patterns, eliminated by explicitouter, except for
   *  occurrences in encoded Switch stmt (=remaining Match(CaseDef(...))
   */
  case class Alternative(trees: List[Tree])
       extends TermTree

  /** Repetition of pattern, eliminated by explicitouter */
  case class Star(elem: Tree)
       extends TermTree

  /** Bind of a variable to a rhs pattern, eliminated by explicitouter
   *
   *  @param name
   *  @param body
   */
  case class Bind(name: Name, body: Tree)
       extends DefTree

  def Bind(sym: Symbol, body: Tree): Bind

  case class UnApply(fun: Tree, args: List[Tree])
       extends TermTree

  /** Array of expressions, needs to be translated in backend,
   */
  case class ArrayValue(elemtpt: Tree, elems: List[Tree])
       extends TermTree

  /** Anonymous function, eliminated by analyzer */
  case class Function(vparams: List[ValDef], body: Tree)
       extends TermTree with SymTree
    // The symbol of a Function is a synthetic value of name nme.ANON_FUN_NAME
    // It is the owner of the function's parameters.

  /** Assignment */
  case class Assign(lhs: Tree, rhs: Tree)
       extends TermTree

  /** Either an assignment or a named argument. Only appears in argument lists,
   *  eliminated by typecheck (doTypedApply)
   */
  case class AssignOrNamedArg(lhs: Tree, rhs: Tree)
       extends TermTree

  /** Conditional expression */
  case class If(cond: Tree, thenp: Tree, elsep: Tree)
       extends TermTree

  /** - Pattern matching expression  (before explicitouter)
   *  - Switch statements            (after explicitouter)
   *
   *  After explicitouter, cases will satisfy the following constraints:
   *
   *  - all guards are `EmptyTree`,
   *  - all patterns will be either `Literal(Constant(x:Int))`
   *    or `Alternative(lit|...|lit)`
   *  - except for an "otherwise" branch, which has pattern
   *    `Ident(nme.WILDCARD)`
   */
  case class Match(selector: Tree, cases: List[CaseDef])
       extends TermTree

  /** Return expression */
  case class Return(expr: Tree)
       extends TermTree with SymTree
    // The symbol of a Return node is the enclosing method.

  case class Try(block: Tree, catches: List[CaseDef], finalizer: Tree)
       extends TermTree

  def Try(body: Tree, cases: (Tree, Tree)*): Try

  /** Throw expression */
  case class Throw(expr: Tree)
       extends TermTree

  def Throw(tpe: Type, args: Tree*): Throw

  /** Object instantiation
   *  One should always use factory method below to build a user level new.
   *
   *  @param tpt    a class type
   */
  case class New(tpt: Tree) extends TermTree

  /** Factory method for object creation `new tpt(args_1)...(args_n)`
   *  A `New(t, as)` is expanded to: `(new t).<init>(as)`
   */
  def New(tpt: Tree, argss: List[List[Tree]]): Tree

  /** 0-1 argument list new, based on a type.
   */
  def New(tpe: Type, args: Tree*): Tree

  def New(sym: Symbol, args: Tree*): Tree

  /** Type annotation, eliminated by explicit outer */
  case class Typed(expr: Tree, tpt: Tree)
       extends TermTree

  /** Common base class for Apply and TypeApply. This could in principle
   *  be a SymTree, but whether or not a Tree is a SymTree isn't used
   *  to settle any interesting questions, and it would add a useless
   *  field to all the instances (useless, since GenericApply forwards to
   *  the underlying fun.)
   */
  abstract class GenericApply extends TermTree {
    val fun: Tree
    val args: List[Tree]
  }

  /** Explicit type application.
   *  @PP: All signs point toward it being a requirement that args.nonEmpty,
   *  but I can't find that explicitly stated anywhere.  Unless your last name
   *  is odersky, you should probably treat it as true.
   */
  case class TypeApply(fun: Tree, args: List[Tree])
       extends GenericApply {

    // Testing the above theory re: args.nonEmpty.
    require(args.nonEmpty, this)
    override def symbol: Symbol = fun.symbol
    override def symbol_=(sym: Symbol) { fun.symbol = sym }
  }

  /** Value application */
  case class Apply(fun: Tree, args: List[Tree])
       extends GenericApply {
    override def symbol: Symbol = fun.symbol
    override def symbol_=(sym: Symbol) { fun.symbol = sym }
  }

  def Apply(sym: Symbol, args: Tree*): Tree

  // TODO remove this class, add a tree attachment to Apply to track whether implicits were involved
  // copying trees will all too easily forget to distinguish subclasses
  class ApplyToImplicitArgs(fun: Tree, args: List[Tree]) extends Apply(fun, args)

  // TODO remove this class, add a tree attachment to Apply to track whether implicits were involved
  // copying trees will all too easily forget to distinguish subclasses
  class ApplyImplicitView(fun: Tree, args: List[Tree]) extends Apply(fun, args)

  // TODO: use a factory method, not a class (???)
  // as a case in point of the comment that should go here by similarity to ApplyToImplicitArgs,
  // this tree is considered in importers, but not in treecopier
  class ApplyConstructor(tpt: Tree, args: List[Tree]) extends Apply(Select(New(tpt), nme.CONSTRUCTOR), args) {
    override def printingPrefix = "ApplyConstructor"
  }

  /** Dynamic value application.
   *  In a dynamic application   q.f(as)
   *   - q is stored in qual
   *   - as is stored in args
   *   - f is stored as the node's symbol field.
   */
  case class ApplyDynamic(qual: Tree, args: List[Tree])
       extends TermTree with SymTree
    // The symbol of an ApplyDynamic is the function symbol of `qual`, or NoSymbol, if there is none.

  /** Super reference, qual = corresponding this reference
   *  A super reference C.super[M] is represented as Super(This(C), M).
   */
  case class Super(qual: Tree, mix: TypeName) extends TermTree {
    // The symbol of a Super is the class _from_ which the super reference is made.
    // For instance in C.super(...), it would be C.
    override def symbol: Symbol = qual.symbol
    override def symbol_=(sym: Symbol) { qual.symbol = sym }
  }

  def Super(sym: Symbol, mix: TypeName): Tree

  /** Self reference */
  case class This(qual: TypeName)
        extends TermTree with SymTree
    // The symbol of a This is the class to which the this refers.
    // For instance in C.this, it would be C.

  def This(sym: Symbol): Tree

  /** Designator <qualifier> . <name> */
  case class Select(qualifier: Tree, name: Name)
       extends RefTree

  def Select(qualifier: Tree, name: String): Select

  def Select(qualifier: Tree, sym: Symbol): Select

  /** Identifier <name> */
  case class Ident(name: Name) extends RefTree {
    def qualifier: Tree = EmptyTree
    def isBackquoted = this hasAttachment BackquotedIdentifier
  }

  def Ident(name: String): Ident

  def Ident(sym: Symbol): Ident

  /** Marks underlying reference to id as boxed.
   *  @pre id must refer to a captured variable
   *  A reference such marked will refer to the boxed entity, no dereferencing
   *  with `.elem` is done on it.
   *  This tree node can be emitted by macros such as reify that call referenceCapturedVariable.
   *  It is eliminated in LambdaLift, where the boxing conversion takes place.
   */
  case class ReferenceToBoxed(ident: Ident) extends TermTree {
    override def symbol: Symbol = ident.symbol
    override def symbol_=(sym: Symbol) { ident.symbol = sym }
  }

  /** Literal */
  case class Literal(value: Constant)
        extends TermTree {
    assert(value ne null)
  }

//  @deprecated("will be removed and then be re-introduced with changed semantics, use Literal(Constant(x)) instead")
//  def Literal(x: Any) = new Literal(Constant(x))

  /** A tree that has an annotation attached to it. Only used for annotated types and
   *  annotation ascriptions, annotations on definitions are stored in the Modifiers.
   *  Eliminated by typechecker (typedAnnotated), the annotations are then stored in
   *  an AnnotatedType.
   */
  case class Annotated(annot: Tree, arg: Tree) extends Tree

  /** Singleton type, eliminated by RefCheck */
  case class SingletonTypeTree(ref: Tree)
        extends TypTree

  /** Type selection <qualifier> # <name>, eliminated by RefCheck */
  case class SelectFromTypeTree(qualifier: Tree, name: TypeName)
       extends TypTree with RefTree

  /** Intersection type <parent1> with ... with <parentN> { <decls> }, eliminated by RefCheck */
  case class CompoundTypeTree(templ: Template)
       extends TypTree

  /** Applied type <tpt> [ <args> ], eliminated by RefCheck */
  case class AppliedTypeTree(tpt: Tree, args: List[Tree])
       extends TypTree {
    override def symbol: Symbol = tpt.symbol
    override def symbol_=(sym: Symbol) { tpt.symbol = sym }
  }

  case class TypeBoundsTree(lo: Tree, hi: Tree)
       extends TypTree

  case class ExistentialTypeTree(tpt: Tree, whereClauses: List[Tree])
       extends TypTree

  /** A synthetic tree holding an arbitrary type.  Not to be confused with
    * with TypTree, the trait for trees that are only used for type trees.
    * TypeTree's are inserted in several places, but most notably in
    * `RefCheck`, where the arbitrary type trees are all replaced by
    * TypeTree's. */
  case class TypeTree() extends TypTree {
    private var orig: Tree = null
    private[scala] var wasEmpty: Boolean = false

    override def symbol = if (tpe == null) null else tpe.typeSymbol
    override def isEmpty = (tpe eq null) || tpe == NoType

    def original: Tree = orig
    def setOriginal(tree: Tree): this.type = {
      def followOriginal(t: Tree): Tree = t match {
        case tt: TypeTree => followOriginal(tt.original)
        case t => t
      }

      orig = followOriginal(tree)
      this setPos tree.pos
    }

    override def defineType(tp: Type): this.type = {
      wasEmpty = isEmpty
      setType(tp)
    }
  }

  def TypeTree(tp: Type): TypeTree = TypeTree() setType tp

  /** An empty deferred value definition corresponding to:
   *    val _: _
   *  This is used as a placeholder in the `self` parameter Template if there is
   *  no definition of a self value of self type.
   */
  def emptyValDef: ValDef

  // ------ traversers, copiers, and transformers ---------------------------------------------

  val treeCopy = newLazyTreeCopier

  def copyDefDef(tree: Tree)(
    mods: Modifiers              = null,
    name: Name                   = null,
    tparams: List[TypeDef]       = null,
    vparamss: List[List[ValDef]] = null,
    tpt: Tree                    = null,
    rhs: Tree                    = null
  ): DefDef = tree match {
    case DefDef(mods0, name0, tparams0, vparamss0, tpt0, rhs0) =>
      treeCopy.DefDef(tree,
        if (mods eq null) mods0 else mods,
        if (name eq null) name0 else name,
        if (tparams eq null) tparams0 else tparams,
        if (vparamss eq null) vparamss0 else vparamss,
        if (tpt eq null) tpt0 else tpt,
        if (rhs eq null) rhs0 else rhs
      )
    case t =>
      sys.error("Not a DefDef: " + t + "/" + t.getClass)
  }
  def copyValDef(tree: Tree)(
    mods: Modifiers = null,
    name: Name      = null,
    tpt: Tree       = null,
    rhs: Tree       = null
  ): ValDef = tree match {
    case ValDef(mods0, name0, tpt0, rhs0) =>
      treeCopy.ValDef(tree,
        if (mods eq null) mods0 else mods,
        if (name eq null) name0 else name,
        if (tpt eq null) tpt0 else tpt,
        if (rhs eq null) rhs0 else rhs
      )
    case t =>
      sys.error("Not a ValDef: " + t + "/" + t.getClass)
  }
  def copyClassDef(tree: Tree)(
    mods: Modifiers        = null,
    name: Name             = null,
    tparams: List[TypeDef] = null,
    impl: Template         = null
  ): ClassDef = tree match {
    case ClassDef(mods0, name0, tparams0, impl0) =>
      treeCopy.ClassDef(tree,
        if (mods eq null) mods0 else mods,
        if (name eq null) name0 else name,
        if (tparams eq null) tparams0 else tparams,
        if (impl eq null) impl0 else impl
      )
    case t =>
      sys.error("Not a ClassDef: " + t + "/" + t.getClass)
  }

  def deriveDefDef(ddef: Tree)(applyToRhs: Tree => Tree): DefDef = ddef match {
    case DefDef(mods0, name0, tparams0, vparamss0, tpt0, rhs0) =>
      treeCopy.DefDef(ddef, mods0, name0, tparams0, vparamss0, tpt0, applyToRhs(rhs0))
    case t =>
      sys.error("Not a DefDef: " + t + "/" + t.getClass)
  }
  def deriveValDef(vdef: Tree)(applyToRhs: Tree => Tree): ValDef = vdef match {
    case ValDef(mods0, name0, tpt0, rhs0) =>
      treeCopy.ValDef(vdef, mods0, name0, tpt0, applyToRhs(rhs0))
    case t =>
      sys.error("Not a ValDef: " + t + "/" + t.getClass)
  }
  def deriveTemplate(templ: Tree)(applyToBody: List[Tree] => List[Tree]): Template = templ match {
    case Template(parents0, self0, body0) =>
      treeCopy.Template(templ, parents0, self0, applyToBody(body0))
    case t =>
      sys.error("Not a Template: " + t + "/" + t.getClass)
  }
  def deriveClassDef(cdef: Tree)(applyToImpl: Template => Template): ClassDef = cdef match {
    case ClassDef(mods0, name0, tparams0, impl0) =>
      treeCopy.ClassDef(cdef, mods0, name0, tparams0, applyToImpl(impl0))
    case t =>
      sys.error("Not a ClassDef: " + t + "/" + t.getClass)
  }
  def deriveModuleDef(mdef: Tree)(applyToImpl: Template => Template): ModuleDef = mdef match {
    case ModuleDef(mods0, name0, impl0) =>
      treeCopy.ModuleDef(mdef, mods0, name0, applyToImpl(impl0))
    case t =>
      sys.error("Not a ModuleDef: " + t + "/" + t.getClass)
  }
  def deriveCaseDef(cdef: Tree)(applyToBody: Tree => Tree): CaseDef = cdef match {
    case CaseDef(pat0, guard0, body0) =>
      treeCopy.CaseDef(cdef, pat0, guard0, applyToBody(body0))
    case t =>
      sys.error("Not a CaseDef: " + t + "/" + t.getClass)
  }
  def deriveLabelDef(ldef: Tree)(applyToRhs: Tree => Tree): LabelDef = ldef match {
    case LabelDef(name0, params0, rhs0) =>
      treeCopy.LabelDef(ldef, name0, params0, applyToRhs(rhs0))
    case t =>
      sys.error("Not a LabelDef: " + t + "/" + t.getClass)
  }

  class Traverser {
    protected var currentOwner: Symbol = definitions.RootClass

    def traverse(tree: Tree): Unit = tree match {
      case EmptyTree =>
        ;
      case PackageDef(pid, stats) =>
        traverse(pid)
        atOwner(tree.symbol.moduleClass) {
          traverseTrees(stats)
        }
      case ClassDef(mods, name, tparams, impl) =>
        atOwner(tree.symbol) {
          traverseTrees(mods.annotations); traverseTrees(tparams); traverse(impl)
        }
      case ModuleDef(mods, name, impl) =>
        atOwner(tree.symbol.moduleClass) {
          traverseTrees(mods.annotations); traverse(impl)
        }
      case ValDef(mods, name, tpt, rhs) =>
        atOwner(tree.symbol) {
          traverseTrees(mods.annotations); traverse(tpt); traverse(rhs)
        }
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        atOwner(tree.symbol) {
          traverseTrees(mods.annotations); traverseTrees(tparams); traverseTreess(vparamss); traverse(tpt); traverse(rhs)
        }
      case TypeDef(mods, name, tparams, rhs) =>
        atOwner(tree.symbol) {
          traverseTrees(mods.annotations); traverseTrees(tparams); traverse(rhs)
        }
      case LabelDef(name, params, rhs) =>
        traverseTrees(params); traverse(rhs)
      case Import(expr, selectors) =>
        traverse(expr)
      case Annotated(annot, arg) =>
        traverse(annot); traverse(arg)
      case Template(parents, self, body) =>
        traverseTrees(parents)
        if (!self.isEmpty) traverse(self)
        traverseStats(body, tree.symbol)
      case Block(stats, expr) =>
        traverseTrees(stats); traverse(expr)
      case CaseDef(pat, guard, body) =>
        traverse(pat); traverse(guard); traverse(body)
      case Alternative(trees) =>
        traverseTrees(trees)
      case Star(elem) =>
        traverse(elem)
      case Bind(name, body) =>
        traverse(body)
      case UnApply(fun, args) =>
        traverse(fun); traverseTrees(args)
      case ArrayValue(elemtpt, trees) =>
        traverse(elemtpt); traverseTrees(trees)
      case Function(vparams, body) =>
        atOwner(tree.symbol) {
          traverseTrees(vparams); traverse(body)
        }
      case Assign(lhs, rhs) =>
        traverse(lhs); traverse(rhs)
      case AssignOrNamedArg(lhs, rhs) =>
        traverse(lhs); traverse(rhs)
      case If(cond, thenp, elsep) =>
        traverse(cond); traverse(thenp); traverse(elsep)
      case Match(selector, cases) =>
        traverse(selector); traverseTrees(cases)
      case Return(expr) =>
        traverse(expr)
      case Try(block, catches, finalizer) =>
        traverse(block); traverseTrees(catches); traverse(finalizer)
      case Throw(expr) =>
        traverse(expr)
      case New(tpt) =>
        traverse(tpt)
      case Typed(expr, tpt) =>
        traverse(expr); traverse(tpt)
      case TypeApply(fun, args) =>
        traverse(fun); traverseTrees(args)
      case Apply(fun, args) =>
        traverse(fun); traverseTrees(args)
      case ApplyDynamic(qual, args) =>
        traverse(qual); traverseTrees(args)
      case Super(qual, _) =>
        traverse(qual)
      case This(_) =>
        ;
      case Select(qualifier, selector) =>
        traverse(qualifier)
      case Ident(_) =>
        ;
      case ReferenceToBoxed(idt) =>
        traverse(idt)
      case Literal(_) =>
        ;
      case TypeTree() =>
        ;
      case SingletonTypeTree(ref) =>
        traverse(ref)
      case SelectFromTypeTree(qualifier, selector) =>
        traverse(qualifier)
      case CompoundTypeTree(templ) =>
        traverse(templ)
      case AppliedTypeTree(tpt, args) =>
        traverse(tpt); traverseTrees(args)
      case TypeBoundsTree(lo, hi) =>
        traverse(lo); traverse(hi)
      case ExistentialTypeTree(tpt, whereClauses) =>
        traverse(tpt); traverseTrees(whereClauses)
      case _ => xtraverse(this, tree)
    }

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

  protected def xtraverse(traverser: Traverser, tree: Tree): Unit = throw new MatchError(tree)

  // to be implemented in subclasses:
  type TreeCopier <: TreeCopierOps
  def newStrictTreeCopier: TreeCopier
  def newLazyTreeCopier: TreeCopier

  trait TreeCopierOps {
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

  class StrictTreeCopier extends TreeCopierOps {
    def ClassDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], impl: Template) =
      new ClassDef(mods, name.toTypeName, tparams, impl).copyAttrs(tree)
    def PackageDef(tree: Tree, pid: RefTree, stats: List[Tree]) =
      new PackageDef(pid, stats).copyAttrs(tree)
    def ModuleDef(tree: Tree, mods: Modifiers, name: Name, impl: Template) =
      new ModuleDef(mods, name.toTermName, impl).copyAttrs(tree)
    def ValDef(tree: Tree, mods: Modifiers, name: Name, tpt: Tree, rhs: Tree) =
      new ValDef(mods, name.toTermName, tpt, rhs).copyAttrs(tree)
    def DefDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree) =
      new DefDef(mods, name.toTermName, tparams, vparamss, tpt, rhs).copyAttrs(tree)
    def TypeDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], rhs: Tree) =
      new TypeDef(mods, name.toTypeName, tparams, rhs).copyAttrs(tree)
    def LabelDef(tree: Tree, name: Name, params: List[Ident], rhs: Tree) =
      new LabelDef(name.toTermName, params, rhs).copyAttrs(tree)
    def Import(tree: Tree, expr: Tree, selectors: List[ImportSelector]) =
      new Import(expr, selectors).copyAttrs(tree)
    def Template(tree: Tree, parents: List[Tree], self: ValDef, body: List[Tree]) =
      new Template(parents, self, body).copyAttrs(tree)
    def Block(tree: Tree, stats: List[Tree], expr: Tree) =
      new Block(stats, expr).copyAttrs(tree)
    def CaseDef(tree: Tree, pat: Tree, guard: Tree, body: Tree) =
      new CaseDef(pat, guard, body).copyAttrs(tree)
    def Alternative(tree: Tree, trees: List[Tree]) =
      new Alternative(trees).copyAttrs(tree)
    def Star(tree: Tree, elem: Tree) =
      new Star(elem).copyAttrs(tree)
    def Bind(tree: Tree, name: Name, body: Tree) =
      new Bind(name, body).copyAttrs(tree)
    def UnApply(tree: Tree, fun: Tree, args: List[Tree]) =
      new UnApply(fun, args).copyAttrs(tree)
    def ArrayValue(tree: Tree, elemtpt: Tree, trees: List[Tree]) =
      new ArrayValue(elemtpt, trees).copyAttrs(tree)
    def Function(tree: Tree, vparams: List[ValDef], body: Tree) =
      new Function(vparams, body).copyAttrs(tree)
    def Assign(tree: Tree, lhs: Tree, rhs: Tree) =
      new Assign(lhs, rhs).copyAttrs(tree)
    def AssignOrNamedArg(tree: Tree, lhs: Tree, rhs: Tree) =
      new AssignOrNamedArg(lhs, rhs).copyAttrs(tree)
    def If(tree: Tree, cond: Tree, thenp: Tree, elsep: Tree) =
      new If(cond, thenp, elsep).copyAttrs(tree)
    def Match(tree: Tree, selector: Tree, cases: List[CaseDef]) =
      new Match(selector, cases).copyAttrs(tree)
    def Return(tree: Tree, expr: Tree) =
      new Return(expr).copyAttrs(tree)
    def Try(tree: Tree, block: Tree, catches: List[CaseDef], finalizer: Tree) =
      new Try(block, catches, finalizer).copyAttrs(tree)
    def Throw(tree: Tree, expr: Tree) =
      new Throw(expr).copyAttrs(tree)
    def New(tree: Tree, tpt: Tree) =
      new New(tpt).copyAttrs(tree)
    def Typed(tree: Tree, expr: Tree, tpt: Tree) =
      new Typed(expr, tpt).copyAttrs(tree)
    def TypeApply(tree: Tree, fun: Tree, args: List[Tree]) =
      new TypeApply(fun, args).copyAttrs(tree)
    def Apply(tree: Tree, fun: Tree, args: List[Tree]) =
      (tree match { // TODO: use a tree attachment to track whether this is an apply to implicit args or a view
        case _: ApplyToImplicitArgs => new ApplyToImplicitArgs(fun, args)
        case _: ApplyImplicitView => new ApplyImplicitView(fun, args)
        // TODO: ApplyConstructor ???
        case _ => new Apply(fun, args)
      }).copyAttrs(tree)
    def ApplyDynamic(tree: Tree, qual: Tree, args: List[Tree]) =
      new ApplyDynamic(qual, args).copyAttrs(tree)
    def Super(tree: Tree, qual: Tree, mix: TypeName) =
      new Super(qual, mix).copyAttrs(tree)
    def This(tree: Tree, qual: Name) =
      new This(qual.toTypeName).copyAttrs(tree)
    def Select(tree: Tree, qualifier: Tree, selector: Name) =
      new Select(qualifier, selector).copyAttrs(tree)
    def Ident(tree: Tree, name: Name) = {
      val t = new Ident(name) copyAttrs tree
      if (tree hasAttachment BackquotedIdentifier) t withAttachment BackquotedIdentifier
      else t
    }
    def ReferenceToBoxed(tree: Tree, idt: Ident) =
      new ReferenceToBoxed(idt).copyAttrs(tree)
    def Literal(tree: Tree, value: Constant) =
      new Literal(value).copyAttrs(tree)
    def TypeTree(tree: Tree) =
      new TypeTree().copyAttrs(tree)
    def Annotated(tree: Tree, annot: Tree, arg: Tree) =
      new Annotated(annot, arg).copyAttrs(tree)
    def SingletonTypeTree(tree: Tree, ref: Tree) =
      new SingletonTypeTree(ref).copyAttrs(tree)
    def SelectFromTypeTree(tree: Tree, qualifier: Tree, selector: Name) =
      new SelectFromTypeTree(qualifier, selector.toTypeName).copyAttrs(tree)
    def CompoundTypeTree(tree: Tree, templ: Template) =
      new CompoundTypeTree(templ).copyAttrs(tree)
    def AppliedTypeTree(tree: Tree, tpt: Tree, args: List[Tree]) =
      new AppliedTypeTree(tpt, args).copyAttrs(tree)
    def TypeBoundsTree(tree: Tree, lo: Tree, hi: Tree) =
      new TypeBoundsTree(lo, hi).copyAttrs(tree)
    def ExistentialTypeTree(tree: Tree, tpt: Tree, whereClauses: List[Tree]) =
      new ExistentialTypeTree(tpt, whereClauses).copyAttrs(tree)
  }

  class LazyTreeCopier extends TreeCopierOps {
    val treeCopy: TreeCopier = newStrictTreeCopier
    def ClassDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], impl: Template) = tree match {
      case t @ ClassDef(mods0, name0, tparams0, impl0)
      if (mods0 == mods) && (name0 == name) && (tparams0 == tparams) && (impl0 == impl) => t
      case _ => treeCopy.ClassDef(tree, mods, name, tparams, impl)
    }
    def PackageDef(tree: Tree, pid: RefTree, stats: List[Tree]) = tree match {
      case t @ PackageDef(pid0, stats0)
      if (pid0 == pid) && (stats0 == stats) => t
      case _ => treeCopy.PackageDef(tree, pid, stats)
    }
    def ModuleDef(tree: Tree, mods: Modifiers, name: Name, impl: Template) = tree match {
      case t @ ModuleDef(mods0, name0, impl0)
      if (mods0 == mods) && (name0 == name) && (impl0 == impl) => t
      case _ => treeCopy.ModuleDef(tree, mods, name, impl)
    }
    def ValDef(tree: Tree, mods: Modifiers, name: Name, tpt: Tree, rhs: Tree) = tree match {
      case t @ ValDef(mods0, name0, tpt0, rhs0)
      if (mods0 == mods) && (name0 == name) && (tpt0 == tpt) && (rhs0 == rhs) => t
      case _ => treeCopy.ValDef(tree, mods, name, tpt, rhs)
    }
    def DefDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree) = tree match {
      case t @ DefDef(mods0, name0, tparams0, vparamss0, tpt0, rhs0)
      if (mods0 == mods) && (name0 == name) && (tparams0 == tparams) &&
         (vparamss0 == vparamss) && (tpt0 == tpt) && (rhs == rhs0) => t
      case _ => treeCopy.DefDef(tree, mods, name, tparams, vparamss, tpt, rhs)
    }
    def TypeDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], rhs: Tree) = tree match {
      case t @ TypeDef(mods0, name0, tparams0, rhs0)
      if (mods0 == mods) && (name0 == name) && (tparams0 == tparams) && (rhs0 == rhs) => t
      case _ => treeCopy.TypeDef(tree, mods, name, tparams, rhs)
    }
    def LabelDef(tree: Tree, name: Name, params: List[Ident], rhs: Tree) = tree match {
      case t @ LabelDef(name0, params0, rhs0)
      if (name0 == name) && (params0 == params) && (rhs0 == rhs) => t
      case _ => treeCopy.LabelDef(tree, name, params, rhs)
    }
    def Import(tree: Tree, expr: Tree, selectors: List[ImportSelector]) = tree match {
      case t @ Import(expr0, selectors0)
      if (expr0 == expr) && (selectors0 == selectors) => t
      case _ => treeCopy.Import(tree, expr, selectors)
    }
    def Template(tree: Tree, parents: List[Tree], self: ValDef, body: List[Tree]) = tree match {
      case t @ Template(parents0, self0, body0)
      if (parents0 == parents) && (self0 == self) && (body0 == body) => t
      case _ => treeCopy.Template(tree, parents, self, body)
    }
    def Block(tree: Tree, stats: List[Tree], expr: Tree) = tree match {
      case t @ Block(stats0, expr0)
      if ((stats0 == stats) && (expr0 == expr)) => t
      case _ => treeCopy.Block(tree, stats, expr)
    }
    def CaseDef(tree: Tree, pat: Tree, guard: Tree, body: Tree) = tree match {
      case t @ CaseDef(pat0, guard0, body0)
      if (pat0 == pat) && (guard0 == guard) && (body0 == body) => t
      case _ => treeCopy.CaseDef(tree, pat, guard, body)
    }
    def Alternative(tree: Tree, trees: List[Tree]) = tree match {
      case t @ Alternative(trees0)
      if trees0 == trees => t
      case _ => treeCopy.Alternative(tree, trees)
    }
    def Star(tree: Tree, elem: Tree) = tree match {
      case t @ Star(elem0)
      if elem0 == elem => t
      case _ => treeCopy.Star(tree, elem)
    }
    def Bind(tree: Tree, name: Name, body: Tree) = tree match {
      case t @ Bind(name0, body0)
      if (name0 == name) && (body0 == body) => t
      case _ => treeCopy.Bind(tree, name, body)
    }
    def UnApply(tree: Tree, fun: Tree, args: List[Tree]) = tree match {
      case t @ UnApply(fun0, args0)
      if (fun0 == fun) && (args0 == args) => t
      case _ => treeCopy.UnApply(tree, fun, args)
    }
    def ArrayValue(tree: Tree, elemtpt: Tree, trees: List[Tree]) = tree match {
      case t @ ArrayValue(elemtpt0, trees0)
      if (elemtpt0 == elemtpt) && (trees0 == trees) => t
      case _ => treeCopy.ArrayValue(tree, elemtpt, trees)
    }
    def Function(tree: Tree, vparams: List[ValDef], body: Tree) = tree match {
      case t @ Function(vparams0, body0)
      if (vparams0 == vparams) && (body0 == body) => t
      case _ => treeCopy.Function(tree, vparams, body)
    }
    def Assign(tree: Tree, lhs: Tree, rhs: Tree) = tree match {
      case t @ Assign(lhs0, rhs0)
      if (lhs0 == lhs) && (rhs0 == rhs) => t
      case _ => treeCopy.Assign(tree, lhs, rhs)
    }
    def AssignOrNamedArg(tree: Tree, lhs: Tree, rhs: Tree) = tree match {
      case t @ AssignOrNamedArg(lhs0, rhs0)
      if (lhs0 == lhs) && (rhs0 == rhs) => t
      case _ => treeCopy.AssignOrNamedArg(tree, lhs, rhs)
    }
    def If(tree: Tree, cond: Tree, thenp: Tree, elsep: Tree) = tree match {
      case t @ If(cond0, thenp0, elsep0)
      if (cond0 == cond) && (thenp0 == thenp) && (elsep0 == elsep) => t
      case _ => treeCopy.If(tree, cond, thenp, elsep)
    }
    def Match(tree: Tree, selector: Tree, cases: List[CaseDef]) =  tree match {
      case t @ Match(selector0, cases0)
      if (selector0 == selector) && (cases0 == cases) => t
      case _ => treeCopy.Match(tree, selector, cases)
    }
    def Return(tree: Tree, expr: Tree) = tree match {
      case t @ Return(expr0)
      if expr0 == expr => t
      case _ => treeCopy.Return(tree, expr)
    }
    def Try(tree: Tree, block: Tree, catches: List[CaseDef], finalizer: Tree) = tree match {
      case t @ Try(block0, catches0, finalizer0)
      if (block0 == block) && (catches0 == catches) && (finalizer0 == finalizer) => t
      case _ => treeCopy.Try(tree, block, catches, finalizer)
    }
    def Throw(tree: Tree, expr: Tree) = tree match {
      case t @ Throw(expr0)
      if expr0 == expr => t
      case _ => treeCopy.Throw(tree, expr)
    }
    def New(tree: Tree, tpt: Tree) = tree match {
      case t @ New(tpt0)
      if tpt0 == tpt => t
      case _ => treeCopy.New(tree, tpt)
    }
    def Typed(tree: Tree, expr: Tree, tpt: Tree) = tree match {
      case t @ Typed(expr0, tpt0)
      if (expr0 == expr) && (tpt0 == tpt) => t
      case _ => treeCopy.Typed(tree, expr, tpt)
    }
    def TypeApply(tree: Tree, fun: Tree, args: List[Tree]) = tree match {
      case t @ TypeApply(fun0, args0)
      if (fun0 == fun) && (args0 == args) => t
      case _ => treeCopy.TypeApply(tree, fun, args)
    }
    def Apply(tree: Tree, fun: Tree, args: List[Tree]) = tree match {
      case t @ Apply(fun0, args0)
      if (fun0 == fun) && (args0 == args) => t
      case _ => treeCopy.Apply(tree, fun, args)
    }
    def ApplyDynamic(tree: Tree, qual: Tree, args: List[Tree]) = tree match {
      case t @ ApplyDynamic(qual0, args0)
      if (qual0 == qual) && (args0 == args) => t
      case _ => treeCopy.ApplyDynamic(tree, qual, args)
    }
    def Super(tree: Tree, qual: Tree, mix: TypeName) = tree match {
      case t @ Super(qual0, mix0)
      if (qual0 == qual) && (mix0 == mix) => t
      case _ => treeCopy.Super(tree, qual, mix)
    }
    def This(tree: Tree, qual: Name) = tree match {
      case t @ This(qual0)
      if qual0 == qual => t
      case _ => treeCopy.This(tree, qual)
    }
    def Select(tree: Tree, qualifier: Tree, selector: Name) = tree match {
      case t @ Select(qualifier0, selector0)
      if (qualifier0 == qualifier) && (selector0 == selector) => t
      case _ => treeCopy.Select(tree, qualifier, selector)
    }
    def Ident(tree: Tree, name: Name) = tree match {
      case t @ Ident(name0)
      if name0 == name => t
      case _ => treeCopy.Ident(tree, name)
    }
    def ReferenceToBoxed(tree: Tree, idt: Ident) = tree match {
      case t @ ReferenceToBoxed(idt0)
      if (idt0 == idt) => t
      case _ => this.treeCopy.ReferenceToBoxed(tree, idt)
    }
    def Literal(tree: Tree, value: Constant) = tree match {
      case t @ Literal(value0)
      if value0 == value => t
      case _ => treeCopy.Literal(tree, value)
    }
    def TypeTree(tree: Tree) = tree match {
      case t @ TypeTree() => t
      case _ => treeCopy.TypeTree(tree)
    }
    def Annotated(tree: Tree, annot: Tree, arg: Tree) = tree match {
      case t @ Annotated(annot0, arg0)
      if (annot0==annot) => t
      case _ => treeCopy.Annotated(tree, annot, arg)
    }
    def SingletonTypeTree(tree: Tree, ref: Tree) = tree match {
      case t @ SingletonTypeTree(ref0)
      if ref0 == ref => t
      case _ => treeCopy.SingletonTypeTree(tree, ref)
    }
    def SelectFromTypeTree(tree: Tree, qualifier: Tree, selector: Name) = tree match {
      case t @ SelectFromTypeTree(qualifier0, selector0)
      if (qualifier0 == qualifier) && (selector0 == selector) => t
      case _ => treeCopy.SelectFromTypeTree(tree, qualifier, selector)
    }
    def CompoundTypeTree(tree: Tree, templ: Template) = tree match {
      case t @ CompoundTypeTree(templ0)
      if templ0 == templ => t
      case _ => treeCopy.CompoundTypeTree(tree, templ)
    }
    def AppliedTypeTree(tree: Tree, tpt: Tree, args: List[Tree]) = tree match {
      case t @ AppliedTypeTree(tpt0, args0)
      if (tpt0 == tpt) && (args0 == args) => t
      case _ => treeCopy.AppliedTypeTree(tree, tpt, args)
    }
    def TypeBoundsTree(tree: Tree, lo: Tree, hi: Tree) = tree match {
      case t @ TypeBoundsTree(lo0, hi0)
      if (lo0 == lo) && (hi0 == hi) => t
      case _ => treeCopy.TypeBoundsTree(tree, lo, hi)
    }
    def ExistentialTypeTree(tree: Tree, tpt: Tree, whereClauses: List[Tree]) = tree match {
      case t @ ExistentialTypeTree(tpt0, whereClauses0)
      if (tpt0 == tpt) && (whereClauses0 == whereClauses) => t
      case _ => treeCopy.ExistentialTypeTree(tree, tpt, whereClauses)
    }
  }

  abstract class Transformer {
    val treeCopy: TreeCopier = newLazyTreeCopier
    protected var currentOwner: Symbol = definitions.RootClass
    protected def currentMethod = currentOwner.enclosingMethod
    protected def currentClass = currentOwner.enclosingClass
    protected def currentPackage = currentOwner.enclosingTopLevelClass.owner
    def transform(tree: Tree): Tree = tree match {
      case EmptyTree =>
        tree
      case PackageDef(pid, stats) =>
        treeCopy.PackageDef(
          tree, transform(pid).asInstanceOf[RefTree],
          atOwner(tree.symbol.moduleClass) {
            transformStats(stats, currentOwner)
          }
        )
      case ClassDef(mods, name, tparams, impl) =>
        atOwner(tree.symbol) {
          treeCopy.ClassDef(tree, transformModifiers(mods), name,
                            transformTypeDefs(tparams), transformTemplate(impl))
        }
      case ModuleDef(mods, name, impl) =>
        atOwner(tree.symbol.moduleClass) {
          treeCopy.ModuleDef(tree, transformModifiers(mods),
                             name, transformTemplate(impl))
        }
      case ValDef(mods, name, tpt, rhs) =>
        atOwner(tree.symbol) {
          treeCopy.ValDef(tree, transformModifiers(mods),
                          name, transform(tpt), transform(rhs))
        }
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        atOwner(tree.symbol) {
          treeCopy.DefDef(tree, transformModifiers(mods), name,
                          transformTypeDefs(tparams), transformValDefss(vparamss),
                          transform(tpt), transform(rhs))
        }
      case TypeDef(mods, name, tparams, rhs) =>
        atOwner(tree.symbol) {
          treeCopy.TypeDef(tree, transformModifiers(mods), name,
                           transformTypeDefs(tparams), transform(rhs))
        }
      case LabelDef(name, params, rhs) =>
        treeCopy.LabelDef(tree, name, transformIdents(params), transform(rhs)) //bq: Martin, once, atOwner(...) works, also change `LamdaLifter.proxy'
      case Import(expr, selectors) =>
        treeCopy.Import(tree, transform(expr), selectors)
      case Template(parents, self, body) =>
        treeCopy.Template(tree, transformTrees(parents), transformValDef(self), transformStats(body, tree.symbol))
      case Block(stats, expr) =>
        treeCopy.Block(tree, transformStats(stats, currentOwner), transform(expr))
      case CaseDef(pat, guard, body) =>
        treeCopy.CaseDef(tree, transform(pat), transform(guard), transform(body))
      case Alternative(trees) =>
        treeCopy.Alternative(tree, transformTrees(trees))
      case Star(elem) =>
        treeCopy.Star(tree, transform(elem))
      case Bind(name, body) =>
        treeCopy.Bind(tree, name, transform(body))
      case UnApply(fun, args) =>
        treeCopy.UnApply(tree, fun, transformTrees(args)) // bq: see test/.../unapplyContexts2.scala
      case ArrayValue(elemtpt, trees) =>
        treeCopy.ArrayValue(tree, transform(elemtpt), transformTrees(trees))
      case Function(vparams, body) =>
        atOwner(tree.symbol) {
          treeCopy.Function(tree, transformValDefs(vparams), transform(body))
        }
      case Assign(lhs, rhs) =>
        treeCopy.Assign(tree, transform(lhs), transform(rhs))
      case AssignOrNamedArg(lhs, rhs) =>
        treeCopy.AssignOrNamedArg(tree, transform(lhs), transform(rhs))
      case If(cond, thenp, elsep) =>
        treeCopy.If(tree, transform(cond), transform(thenp), transform(elsep))
      case Match(selector, cases) =>
        treeCopy.Match(tree, transform(selector), transformCaseDefs(cases))
      case Return(expr) =>
        treeCopy.Return(tree, transform(expr))
      case Try(block, catches, finalizer) =>
        treeCopy.Try(tree, transform(block), transformCaseDefs(catches), transform(finalizer))
      case Throw(expr) =>
        treeCopy.Throw(tree, transform(expr))
      case New(tpt) =>
        treeCopy.New(tree, transform(tpt))
      case Typed(expr, tpt) =>
        treeCopy.Typed(tree, transform(expr), transform(tpt))
      case TypeApply(fun, args) =>
        treeCopy.TypeApply(tree, transform(fun), transformTrees(args))
      case Apply(fun, args) =>
        treeCopy.Apply(tree, transform(fun), transformTrees(args))
      case ApplyDynamic(qual, args) =>
        treeCopy.ApplyDynamic(tree, transform(qual), transformTrees(args))
      case Super(qual, mix) =>
        treeCopy.Super(tree, transform(qual), mix)
      case This(qual) =>
        treeCopy.This(tree, qual)
      case Select(qualifier, selector) =>
        treeCopy.Select(tree, transform(qualifier), selector)
      case Ident(name) =>
        treeCopy.Ident(tree, name)
      case ReferenceToBoxed(idt) =>
        treeCopy.ReferenceToBoxed(tree, transform(idt) match { case idt1: Ident => idt1 })
      case Literal(value) =>
        treeCopy.Literal(tree, value)
      case TypeTree() =>
        treeCopy.TypeTree(tree)
      case Annotated(annot, arg) =>
        treeCopy.Annotated(tree, transform(annot), transform(arg))
      case SingletonTypeTree(ref) =>
        treeCopy.SingletonTypeTree(tree, transform(ref))
      case SelectFromTypeTree(qualifier, selector) =>
        treeCopy.SelectFromTypeTree(tree, transform(qualifier), selector)
      case CompoundTypeTree(templ) =>
        treeCopy.CompoundTypeTree(tree, transformTemplate(templ))
      case AppliedTypeTree(tpt, args) =>
        treeCopy.AppliedTypeTree(tree, transform(tpt), transformTrees(args))
      case TypeBoundsTree(lo, hi) =>
        treeCopy.TypeBoundsTree(tree, transform(lo), transform(hi))
      case ExistentialTypeTree(tpt, whereClauses) =>
        treeCopy.ExistentialTypeTree(tree, transform(tpt), transformTrees(whereClauses))
      case _ =>
        xtransform(this, tree)
    }

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

  protected def xtransform(transformer: Transformer, tree: Tree): Tree = throw new MatchError(tree)

  class ForeachTreeTraverser(f: Tree => Unit) extends Traverser {
    override def traverse(t: Tree) {
      f(t)
      super.traverse(t)
    }
  }

  class FilterTreeTraverser(p: Tree => Boolean) extends Traverser {
    val hits = new ListBuffer[Tree]
    override def traverse(t: Tree) {
      if (p(t)) hits += t
      super.traverse(t)
    }
  }

  class CollectTreeTraverser[T](pf: PartialFunction[Tree, T]) extends Traverser {
    val results = new ListBuffer[T]
    override def traverse(t: Tree) {
      if (pf.isDefinedAt(t)) results += pf(t)
      super.traverse(t)
    }
  }

  class FindTreeTraverser(p: Tree => Boolean) extends Traverser {
    var result: Option[Tree] = None
    override def traverse(t: Tree) {
      if (result.isEmpty) {
        if (p(t)) result = Some(t)
        super.traverse(t)
      }
    }
  }

  protected def duplicateTree(tree: Tree): Tree

/* A standard pattern match
  case EmptyTree =>
  case PackageDef(pid, stats) =>
     // package pid { stats }
  case ClassDef(mods, name, tparams, impl) =>
     // mods class name [tparams] impl   where impl = extends parents { defs }
  case ModuleDef(mods, name, impl) =>                             (eliminated by refcheck)
     // mods object name impl  where impl = extends parents { defs }
  case ValDef(mods, name, tpt, rhs) =>
     // mods val name: tpt = rhs
     // note missing type information is expressed by tpt = TypeTree()
  case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
     // mods def name[tparams](vparams_1)...(vparams_n): tpt = rhs
     // note missing type information is expressed by tpt = TypeTree()
  case TypeDef(mods, name, tparams, rhs) =>                       (eliminated by erasure)
     // mods type name[tparams] = rhs
     // mods type name[tparams] >: lo <: hi,  where lo, hi are in a TypeBoundsTree,
                                              and DEFERRED is set in mods
  case LabelDef(name, params, rhs) =>
     // used for tailcalls and like
     // while/do are desugared to label defs as follows:
     // while (cond) body ==> LabelDef($L, List(), if (cond) { body; L$() } else ())
     // do body while (cond) ==> LabelDef($L, List(), body; if (cond) L$() else ())
  case Import(expr, selectors) =>                                 (eliminated by typecheck)
     // import expr.{selectors}
     // Selectors are a list of pairs of names (from, to).
     // The last (and maybe only name) may be a nme.WILDCARD
     // for instance
     //   import qual.{x, y => z, _}  would be represented as
     //   Import(qual, List(("x", "x"), ("y", "z"), (WILDCARD, null)))
  case Template(parents, self, body) =>
     // extends parents { self => body }
     // if self is missing it is represented as emptyValDef
  case Block(stats, expr) =>
     // { stats; expr }
  case CaseDef(pat, guard, body) =>                               (eliminated by transmatch/explicitouter)
    // case pat if guard => body
  case Alternative(trees) =>                                      (eliminated by transmatch/explicitouter)
    // pat1 | ... | patn
  case Star(elem) =>                                              (eliminated by transmatch/explicitouter)
    // pat*
  case Bind(name, body) =>                                        (eliminated by transmatch/explicitouter)
    // name @ pat
  case UnApply(fun: Tree, args)                                   (introduced by typer, eliminated by transmatch/explicitouter)
    // used for unapply's
  case ArrayValue(elemtpt, trees) =>                              (introduced by uncurry)
    // used to pass arguments to vararg arguments
    // for instance, printf("%s%d", foo, 42) is translated to after uncurry to:
    // Apply(
    //   Ident("printf"),
    //   Literal("%s%d"),
    //   ArrayValue(<Any>, List(Ident("foo"), Literal(42))))
  case Function(vparams, body) =>                                 (eliminated by lambdaLift)
    // vparams => body  where vparams:List[ValDef]
  case Assign(lhs, rhs) =>
    // lhs = rhs
  case AssignOrNamedArg(lhs, rhs) =>                              (eliminated by typer, resurrected by reifier)
    // @annotation(lhs = rhs)
  case If(cond, thenp, elsep) =>
    // if (cond) thenp else elsep
  case Match(selector, cases) =>
    // selector match { cases }
  case Return(expr) =>
    // return expr
  case Try(block, catches, finalizer) =>
    // try block catch { catches } finally finalizer where catches: List[CaseDef]
  case Throw(expr) =>
    // throw expr
  case New(tpt) =>
    // new tpt   always in the context: (new tpt).<init>[targs](args)
  case Typed(expr, tpt) =>                                        (eliminated by erasure)
    // expr: tpt
  case TypeApply(fun, args) =>
    // fun[args]
  case Apply(fun, args) =>
    // fun(args)
    // for instance fun[targs](args)  is expressed as  Apply(TypeApply(fun, targs), args)
  case ApplyDynamic(qual, args)                                   (introduced by erasure, eliminated by cleanup)
    // fun(args)
  case Super(qual, mix) =>
    // qual.super[mix]     qual is always This(something), if mix is empty, it is tpnme.EMPTY
  case This(qual) =>
    // qual.this
  case Select(qualifier, selector) =>
    // qualifier.selector
  case Ident(name) =>
    // name
    // note: type checker converts idents that refer to enclosing fields or methods
    // to selects; name ==> this.name
  case ReferenceToBoxed(ident) =>                                 (created by typer, eliminated by lambdalift)
    // synthetic node emitted by macros to reference capture vars directly without going through ``elem''
    // var x = ...; fun { x } will emit Ident(x), which gets transformed to Select(Ident(x), "elem")
    // if ReferenceToBoxed were used instead of Ident, no transformation would be performed
  case Literal(value) =>
    // value
  case TypeTree() =>                                              (introduced by refcheck)
    // a type that's not written out, but given in the tpe attribute
  case Annotated(annot, arg) =>                                   (eliminated by typer)
    // arg @annot  for types,  arg: @annot for exprs
  case SingletonTypeTree(ref) =>                                  (eliminated by uncurry)
    // ref.type
  case SelectFromTypeTree(qualifier, selector) =>                 (eliminated by uncurry)
    // qualifier # selector, a path-dependent type p.T is expressed as p.type # T
  case CompoundTypeTree(templ: Template) =>                       (eliminated by uncurry)
    // parent1 with ... with parentN { refinement }
  case AppliedTypeTree(tpt, args) =>                              (eliminated by uncurry)
    // tpt[args]
  case TypeBoundsTree(lo, hi) =>                                  (eliminated by uncurry)
    // >: lo <: hi
  case ExistentialTypeTree(tpt, whereClauses) =>                  (eliminated by uncurry)
    // tpt forSome { whereClauses }
*/
}

