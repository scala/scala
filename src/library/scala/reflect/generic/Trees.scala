package scala.reflect
package generic

import java.io.{PrintWriter, StringWriter}
import Flags._

trait Trees { self: Universe =>

  abstract class AbsTreePrinter(out: PrintWriter) {
    def print(tree: Tree)
    def flush()
  }

  def newTreePrinter(out: PrintWriter): AbsTreePrinter

  private[scala] var nodeCount = 0

  /** @param privateWithin the qualifier for a private (a type name)
   *    or nme.EMPTY.toTypeName, if none is given.
   *  @param annotations the annotations for the definition.
   *    <strong>Note:</strong> the typechecker drops these annotations,
   *    use the AnnotationInfo's (Symbol.annotations) in later phases.
   */
  case class Modifiers(flags: Long, privateWithin: Name, annotations: List[Tree], positions: Map[Long, Position]) {
    def isAbstract      = hasFlag(ABSTRACT )
    def isAccessor      = hasFlag(ACCESSOR )
    def isArgument      = hasFlag(PARAM    )
    def isCase          = hasFlag(CASE     )
    def isContravariant = hasFlag(CONTRAVARIANT)  // marked with `-'
    def isCovariant     = hasFlag(COVARIANT    )  // marked with `+'
    def isDeferred      = hasFlag(DEFERRED )
    def isFinal         = hasFlag(FINAL    )
    def isImplicit      = hasFlag(IMPLICIT )
    def isLazy          = hasFlag(LAZY     )
    def isOverride      = hasFlag(OVERRIDE )
    def isPrivate       = hasFlag(PRIVATE  )
    def isProtected     = hasFlag(PROTECTED)
    def isPublic        = !isPrivate && !isProtected
    def isSealed        = hasFlag(SEALED   )
    def isTrait         = hasFlag(TRAIT    )
    def isVariable      = hasFlag(MUTABLE  )

    def hasFlag(flag: Long) = (flag & flags) != 0L
    def & (flag: Long): Modifiers = {
      val flags1 = flags & flag
      if (flags1 == flags) this
      else Modifiers(flags1, privateWithin, annotations, positions)
    }
    def &~ (flag: Long): Modifiers = {
      val flags1 = flags & (~flag)
      if (flags1 == flags) this
      else Modifiers(flags1, privateWithin, annotations, positions)
    }
    def | (flag: Long): Modifiers = {
      val flags1 = flags | flag
      if (flags1 == flags) this
      else Modifiers(flags1, privateWithin, annotations, positions)
    }
    def withAnnotations(annots: List[Tree]) =
      if (annots.isEmpty) this
      else copy(annotations = annotations ::: annots)
    def withPosition(flag: Long, position: Position) =
      copy(positions = positions + (flag -> position))
  }

  def Modifiers(flags: Long, privateWithin: Name): Modifiers = Modifiers(flags, privateWithin, List(), Map.empty)
  def Modifiers(flags: Long): Modifiers = Modifiers(flags, mkTypeName(nme.EMPTY))

  lazy val NoMods = Modifiers(0)

  abstract class Tree extends Product {
    val id = nodeCount
    nodeCount += 1

    private[this] var rawpos: Position = NoPosition

    def pos = rawpos
    def pos_=(pos: Position) = rawpos = pos
    def setPos(pos: Position): this.type = { rawpos = pos; this }

    private[this] var rawtpe: Type = _

    def tpe = rawtpe
    def tpe_=(t: Type) = rawtpe = t

    /** Set tpe to give `tp` and return this.
     */
    def setType(tp: Type): this.type = { rawtpe = tp; this }

    /** Like `setType`, but if this is a previously empty TypeTree
     *  that fact is remembered so that resetType will snap back.
     */
    def defineType(tp: Type): this.type = setType(tp)

    def symbol: Symbol = null
    def symbol_=(sym: Symbol) { throw new UnsupportedOperationException("symbol_= inapplicable for " + this) }
    def setSymbol(sym: Symbol): this.type = { symbol = sym; this }

    def hasSymbol = false
    def isDef = false
    def isEmpty = false

    /** The direct child trees of this tree
     *  EmptyTrees are always omitted. Lists are collapsed.
     */
    def children: List[Tree] = {
      def subtrees(x: Any): List[Tree] = x match {
        case EmptyTree => List()
        case t: Tree => List(t)
        case xs: List[_] => xs flatMap subtrees
        case _ => List()
      }
      productIterator.toList flatMap subtrees
    }

    /** In compiler: Make a copy of this tree, keeping all attributes,
     *  except that all positions are focussed (so nothing
     *  in this tree will be found when searching by position).
     *  If not in compiler may also return tree unchanged.
     */
    private[scala] def duplicate: this.type =
      duplicateTree(this).asInstanceOf[this.type]

    private[scala] def copyAttrs(tree: Tree): this.type = {
      pos = tree.pos
      tpe = tree.tpe
      if (hasSymbol) symbol = tree.symbol
      this
    }

    override def toString(): String = {
      val buffer = new StringWriter()
      val printer = newTreePrinter(new PrintWriter(buffer))
      printer.print(this)
      printer.flush()
      buffer.toString
    }

    override def hashCode(): Int = super.hashCode()

    override def equals(that: Any): Boolean = that match {
      case t: Tree => this eq t
      case _ => false
    }
  }

  private[scala] def duplicateTree(tree: Tree): Tree = tree

  trait SymTree extends Tree {
    override def hasSymbol = true
    override var symbol: Symbol = NoSymbol
  }

  trait RefTree extends SymTree {
    def name: Name
  }

  abstract class DefTree extends SymTree {
    def name: Name
    override def isDef = true
  }

  trait TermTree extends Tree

  /** A tree for a type.  Note that not all type trees implement
    * this trait; in particular, Ident's are an exception. */
  trait TypTree extends Tree

// ----- tree node alternatives --------------------------------------

  /** The empty tree */
  case object EmptyTree extends TermTree {
    super.tpe_=(NoType)
    override def tpe_=(t: Type) =
      if (t != NoType) throw new UnsupportedOperationException("tpe_=("+t+") inapplicable for <empty>")
    override def isEmpty = true
  }

  abstract class MemberDef extends DefTree {
    def mods: Modifiers
    def keyword: String = this match {
      case TypeDef(_, _, _, _)      => "type"
      case ClassDef(mods, _, _, _)  => if (mods.isTrait) "trait" else "class"
      case DefDef(_, _, _, _, _, _) => "def"
      case ModuleDef(_, _, _)       => "object"
      case PackageDef(_, _)         => "package"
      case ValDef(mods, _, _, _)    => if (mods.isVariable) "var" else "val"
      case _ => ""
    }
    final def hasFlag(mask: Long): Boolean = (mods.flags & mask) != 0L
  }

  /** Package clause
   */
  case class PackageDef(pid: RefTree, stats: List[Tree])
       extends MemberDef {
    def name = pid.name
    def mods = NoMods
  }

  abstract class ImplDef extends MemberDef {
    def impl: Template
  }

  /** Class definition */
  case class ClassDef(mods: Modifiers, name: Name, tparams: List[TypeDef], impl: Template)
       extends ImplDef

  /** Singleton object definition
   */
  case class ModuleDef(mods: Modifiers, name: Name, impl: Template)
       extends ImplDef

  abstract class ValOrDefDef extends MemberDef {
    def tpt: Tree
    def rhs: Tree
  }

  /** Value definition
   */
  case class ValDef(mods: Modifiers, name: Name, tpt: Tree, rhs: Tree) extends ValOrDefDef

  /** Method definition
   */
  case class DefDef(mods: Modifiers, name: Name, tparams: List[TypeDef],
                    vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree) extends ValOrDefDef

  /** Abstract type, type parameter, or type alias */
  case class TypeDef(mods: Modifiers, name: Name, tparams: List[TypeDef], rhs: Tree)
       extends MemberDef

  /** <p>
   *    Labelled expression - the symbols in the array (must be Idents!)
   *    are those the label takes as argument
   *  </p>
   *  <p>
   *    The symbol that is given to the labeldef should have a MethodType
   *    (as if it were a nested function)
   *  </p>
   *  <p>
   *    Jumps are apply nodes attributed with label symbol, the arguments
   *    will get assigned to the idents.
   *  </p>
   *  <p>
   *  Note: on 2005-06-09 Martin, Iuli, Burak agreed to have forward
   *        jumps within a Block.
   *  </p>
   */
  case class LabelDef(name: Name, params: List[Ident], rhs: Tree)
       extends DefTree with TermTree


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
    // System.err.println("TEMPLATE: " + parents)
  }

  /** Block of expressions (semicolon separated expressions) */
  case class Block(stats: List[Tree], expr: Tree)
       extends TermTree

  /** Case clause in a pattern match, eliminated by TransMatch
   *  (except for occurrences in switch statements)
   */
  case class CaseDef(pat: Tree, guard: Tree, body: Tree)
       extends Tree

  /** Alternatives of patterns, eliminated by TransMatch, except for
   *  occurrences in encoded Switch stmt (=remaining Match(CaseDef(...))
   */
  case class Alternative(trees: List[Tree])
       extends TermTree

  /** Repetition of pattern, eliminated by TransMatch */
  case class Star(elem: Tree)
       extends TermTree

  /** Bind of a variable to a rhs pattern, eliminated by TransMatch
   *
   *  @param name
   *  @param body
   */
  case class Bind(name: Name, body: Tree)
       extends DefTree

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

  /** Conditional expression */
  case class If(cond: Tree, thenp: Tree, elsep: Tree)
       extends TermTree

  /** <p>
   *    Pattern matching expression  (before <code>TransMatch</code>)
   *    Switch statements            (after TransMatch)
   *  </p>
   *  <p>
   *    After <code>TransMatch</code>, cases will satisfy the following
   *    constraints:
   *  </p>
   *  <ul>
   *    <li>all guards are EmptyTree,</li>
   *    <li>all patterns will be either <code>Literal(Constant(x:Int))</code>
   *      or <code>Alternative(lit|...|lit)</code></li>
   *    <li>except for an "otherwise" branch, which has pattern
   *      <code>Ident(nme.WILDCARD)</code></li>
   *  </ul>
   */
  case class Match(selector: Tree, cases: List[CaseDef])
       extends TermTree

  /** Return expression */
  case class Return(expr: Tree)
       extends TermTree with SymTree
    // The symbol of a Return node is the enclosing method.

  case class Try(block: Tree, catches: List[CaseDef], finalizer: Tree)
       extends TermTree

  /** Throw expression */
  case class Throw(expr: Tree)
       extends TermTree

  /** Object instantiation
   *  One should always use factory method below to build a user level new.
   *
   *  @param tpt    a class type
   */
  case class New(tpt: Tree) extends TermTree

  /** Type annotation, eliminated by explicit outer */
  case class Typed(expr: Tree, tpt: Tree)
       extends TermTree

  // Martin to Sean: Should GenericApply/TypeApply/Apply not be SymTree's? After all,
  // ApplyDynamic is a SymTree.
  abstract class GenericApply extends TermTree {
    val fun: Tree
    val args: List[Tree]
  }

  /** Type application */
  case class TypeApply(fun: Tree, args: List[Tree])
       extends GenericApply {
    override def symbol: Symbol = fun.symbol
    override def symbol_=(sym: Symbol) { fun.symbol = sym }
  }

  /** Value application */
  case class Apply(fun: Tree, args: List[Tree])
       extends GenericApply {
    override def symbol: Symbol = fun.symbol
    override def symbol_=(sym: Symbol) { fun.symbol = sym }
  }

  /** Dynamic value application.
   *  In a dynamic application   q.f(as)
   *   - q is stored in qual
   *   - as is stored in args
   *   - f is stored as the node's symbol field.
   */
  case class ApplyDynamic(qual: Tree, args: List[Tree])
       extends TermTree with SymTree
    // The symbol of an ApplyDynamic is the function symbol of `qual', or NoSymbol, if there is none.

  /** Super reference */
  case class Super(qual: Name, mix: Name)
       extends TermTree with SymTree
    // The symbol of a Super is the class _from_ which the super reference is made.
    // For instance in C.super(...), it would be C.

  /** Self reference */
  case class This(qual: Name)
        extends TermTree with SymTree
    // The symbol of a This is the class to which the this refers.
    // For instance in C.this, it would be C.

  /** Designator <qualifier> . <name> */
  case class Select(qualifier: Tree, name: Name)
       extends RefTree

  /** Identifier <name> */
  case class Ident(name: Name)
       extends RefTree

  class BackQuotedIdent(name: Name) extends Ident(name)

  /** Literal */
  case class Literal(value: Constant)
        extends TermTree {
    assert(value ne null)
  }

  def Literal(value: Any): Literal =
    Literal(Constant(value))

  type TypeTree <: AbsTypeTree
  val TypeTree: TypeTreeExtractor

  abstract class TypeTreeExtractor {
    def apply(): TypeTree
    def unapply(tree: TypeTree): Boolean
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
      case Super(_, _) =>
        ;
      case This(_) =>
        ;
      case Select(qualifier, selector) =>
        traverse(qualifier)
      case Ident(_) =>
        ;
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
      case SelectFromArray(qualifier, selector, erasure) =>
        traverse(qualifier)
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
  }

  /** A synthetic term holding an arbitrary type.  Not to be confused with
    * with TypTree, the trait for trees that are only used for type trees.
    * TypeTree's are inserted in several places, but most notably in
    * <code>RefCheck</code>, where the arbitrary type trees are all replaced by
    * TypeTree's. */
  abstract class AbsTypeTree extends TypTree {
    override def symbol = if (tpe == null) null else tpe.typeSymbol
    override def isEmpty = (tpe eq null) || tpe == NoType
  }

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
  case class SelectFromTypeTree(qualifier: Tree, name: Name)
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

  /** Array selection <qualifier> . <name> only used during erasure */
  case class SelectFromArray(qualifier: Tree, name: Name, erasure: Type)
       extends TermTree with RefTree { }

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
    // qual.super[mix]     if qual and/or mix is empty, ther are nme.EMPTY.toTypeName
  case This(qual) =>
    // qual.this
  case Select(qualifier, selector) =>
    // qualifier.selector
  case Ident(name) =>
    // name
    // note: type checker converts idents that refer to enclosing fields or methods
    // to selects; name ==> this.name
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
