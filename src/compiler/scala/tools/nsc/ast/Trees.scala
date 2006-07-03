/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.ast

import java.io.{PrintWriter, StringWriter}
import scala.tools.nsc.symtab.Flags
import scala.tools.nsc.util.{HashSet,Position, SourceFile}
import symtab.Flags._


trait Trees requires Global {
  //statistics
  var nodeCount = 0

  case class Modifiers(flags: int, privateWithin: Name) {
    def isPrivate   = (flags & PRIVATE  ) != 0
    def isProtected = (flags & PROTECTED) != 0
    def isVariable  = (flags & MUTABLE  ) != 0
    def isArgument  = (flags & PARAM    ) != 0
    def isAccessor  = (flags & ACCESSOR ) != 0
    def isOverride  = (flags & OVERRIDE ) != 0
    def isAbstract  = (flags & ABSTRACT ) != 0
    def isCase      = (flags & CASE     ) != 0
    def isSealed    = (flags & SEALED   ) != 0
    def isFinal     = (flags & FINAL    ) != 0
    def isTrait     = (flags & TRAIT    ) != 0
    def isPublic    = !isPrivate && !isProtected
    def hasFlag(flag: int) = (flags & flag) != 0
    def | (flag: int): Modifiers = {
      val flags1 = flags | flag
      if (flags1 == flags) this else Modifiers(flags1, privateWithin)
    }
  }

  def Modifiers(flags: int): Modifiers = Modifiers(flags, nme.EMPTY.toTypeName)
  def Modifiers(flags: long): Modifiers = Modifiers(flags.asInstanceOf[int])

  val NoMods = Modifiers(0)

  abstract class Tree {

    if (util.Statistics.enabled) nodeCount = nodeCount + 1

    private var posx: int = Position.NOPOS

    def pos = posx

    var tpe: Type = _

    def setPos(p: int): this.type = { posx = p; this }
    def setType(tp: Type): this.type = { tpe = tp; this }

    def symbol: Symbol = null
    def symbol_=(sym: Symbol): unit =
      throw new Error("symbol_= inapplicable for " + this);
    def setSymbol(sym: Symbol): this.type = { symbol = sym; this }

    def hasSymbol = false
    def isDef = false
    def isTerm = false
    def isType = false
    def isEmpty = false

    def isErroneous = tpe != null && tpe.isErroneous

    override def toString(): String = {
      val buffer = new StringWriter()
      val printer = treePrinters.create(new PrintWriter(buffer))
      printer.print(this); printer.flush
      buffer.toString()
    }

    override def hashCode(): int = super.hashCode()

    override def equals(that: Any): boolean = that match {
      case t: Tree => this eq t
      case _ => false
    }

    def duplicate: this.type = (duplicator transform this).asInstanceOf[this.type]

    def copyAttrs(tree: Tree): this.type = {
      posx = tree.posx
      tpe = tree.tpe
      if (hasSymbol) symbol = tree.symbol
      this
    }
  }

  trait SymTree extends Tree {
    override def hasSymbol = true
    override var symbol: Symbol = NoSymbol
  }

  abstract class DefTree extends SymTree {
    def name: Name
    override def isDef = true
  }

  trait TermTree extends Tree {
    override def isTerm = true
  }

  trait TypTree extends Tree {
    override def isType = true
  }

// ----- auxiliary objects and methods ------------------------------

  private val duplicator = new Transformer {
    override val copy = new StrictTreeCopier
  }

  private def syntheticParams(owner: Symbol, formals: List[Type]): List[Symbol] = {
    var cnt = 0
    def freshName() = { cnt = cnt + 1; newTermName("x$" + cnt) }
    for (val formal <- formals) yield
      owner.newValueParameter(owner.pos, freshName()).setInfo(formal)
  }

  private def syntheticParams(owner: Symbol, mtp: Type): List[List[Symbol]] = mtp match {
    case PolyType(_, restp) =>
      syntheticParams(owner, restp)
    case MethodType(formals, restp) =>
      syntheticParams(owner, formals) :: syntheticParams(owner, restp)
    case _ =>
      List()
  }

//  def nextPhase = if (phase.id > globalPhase.id) phase else phase.next;

// ----- tree node alternatives --------------------------------------

  /** The empty tree */
  case object EmptyTree extends TermTree {
    tpe = NoType
    override def isEmpty = true
  }

  abstract class MemberDef extends DefTree {
    def mods: Modifiers
    def keyword: String = this match {
      case AliasTypeDef(_, _, _, _) => "type"
      case ClassDef(_, _, _, _, _)  => "class"
      case DefDef(_, _, _, _, _, _) => "def"
      case ModuleDef(_, _, _)       => "object"
      case PackageDef(_, _)         => "package"
      case ValDef(mods, _, _, _)    => if (mods.isVariable) "var" else "val"
      case _ => ""
    }
    final def hasFlag(mask: long): boolean = (mods.flags & mask) != 0
  }

  /** Package clause */
  case class PackageDef(name: Name, stats: List[Tree])
       extends MemberDef {
    def mods = NoMods
  }

  def PackageDef(sym: Symbol, stats: List[Tree]): PackageDef =
    PackageDef(sym.name, stats) setSymbol sym

  abstract class ImplDef extends MemberDef {
    def impl: Template
  }

  /** Class definition */
  case class ClassDef(mods: Modifiers, name: Name, tparams: List[AbsTypeDef], tpt: Tree, impl: Template)
       extends ImplDef

  def ClassDef(sym: Symbol, impl: Template): ClassDef =
    posAssigner.atPos(sym.pos) {
      ClassDef(Modifiers(sym.flags),
               sym.name,
               sym.typeParams map AbsTypeDef,
               if (sym.thisSym == sym) EmptyTree else TypeTree(sym.typeOfThis),
               impl) setSymbol sym
    }

  /** Construct class definition with given class symbol, value parameters,
   *  supercall arguments and template body.
   *
   *  @param sym       the class symbol
   *  @param vparamss  the value parameters -- if they have symbols they
   *                   should be owned by `sym'
   *  @param argss     the supercall arguments
   *  @param body      the template statements without primary constructor
   *                   and value parameter fields.
   */
  def ClassDef(sym: Symbol, vparamss: List[List[ValDef]], argss: List[List[Tree]], body: List[Tree]): ClassDef =
    ClassDef(sym, Template(sym.info.parents map TypeTree, vparamss, argss, body))

  /** Singleton object definition
   *
   *  @param mods
   *  @param name
   *  @param impl
   */
  case class ModuleDef(mods: Modifiers, name: Name, impl: Template)
       extends ImplDef

  def ModuleDef(sym: Symbol, impl: Template): ModuleDef =
    posAssigner.atPos(sym.pos) {
      ModuleDef(Modifiers(sym.flags), sym.name, impl)
    }


  abstract class ValOrDefDef extends MemberDef {
    def tpt: Tree
    def rhs: Tree
  }

  /** Value definition
   *
   *  @param mods
   *  @param name
   *  @param tpt
   *  @param rhs
   */
  case class ValDef(mods: Modifiers, name: Name, tpt: Tree, rhs: Tree)
       extends ValOrDefDef {
    assert(tpt.isType, tpt)
    assert(rhs.isTerm, rhs)
  }

  def ValDef(sym: Symbol, rhs: Tree): ValDef = {
    posAssigner.atPos(sym.pos) {
      ValDef(Modifiers(sym.flags), sym.name, TypeTree(sym.tpe), rhs) setSymbol sym
    }
  }

  def ValDef(sym: Symbol): ValDef = ValDef(sym, EmptyTree)

  /** Method definition
   *
   *  @param mods
   *  @param name
   *  @param tparams
   *  @param vparamss
   *  @param tpt
   *  @param rhs
   */
  case class DefDef(mods: Modifiers, name: Name, tparams: List[AbsTypeDef],
                    vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree)
       extends ValOrDefDef {
    assert(tpt.isType)
    assert(rhs.isTerm)
  }

  def DefDef(sym: Symbol, vparamss: List[List[ValDef]], rhs: Tree): DefDef =
    posAssigner.atPos(sym.pos) {
      assert(sym != NoSymbol)
      DefDef(Modifiers(sym.flags),
             sym.name,
             sym.typeParams map AbsTypeDef,
             vparamss,
             TypeTree(sym.tpe.finalResultType),
             rhs) setSymbol sym
    }

  def DefDef(sym: Symbol, rhs: List[List[Symbol]] => Tree): DefDef = {
    val vparamss = syntheticParams(sym, sym.tpe)
    DefDef(sym, vparamss map (.map(ValDef)), rhs(vparamss))
  }

  /** Abstract type or type parameter
   *
   *  @param mods
   *  @param name
   *  @param lo
   *  @param hi
   */
  case class AbsTypeDef(mods: Modifiers, name: Name, lo: Tree, hi: Tree)
       extends DefTree {
    def namePos = pos - name.length
  }

  def AbsTypeDef(sym: Symbol): AbsTypeDef =
    posAssigner.atPos(sym.pos) {
      AbsTypeDef(Modifiers(sym.flags), sym.name,
                 TypeTree(sym.info.bounds.lo), TypeTree(sym.info.bounds.hi))
    }

  /** Type alias
   *
   *  @param mods
   *  @param name
   *  @param tparams
   *  @param rhs
   */
  case class AliasTypeDef(mods: Modifiers, name: Name, tparams: List[AbsTypeDef], rhs: Tree)
       extends MemberDef

  def AliasTypeDef(sym: Symbol, rhs: Tree): AliasTypeDef =
    posAssigner.atPos(sym.pos) {
      AliasTypeDef(Modifiers(sym.flags), sym.name, sym.typeParams map AbsTypeDef, rhs)
    }

  /** Labelled expression - the symbols in the array (must be Idents!)
   *  are those the label takes as argument
   *
   *  The symbol that is given to the labeldef should have a MethodType
   *  (as if it were a nested function)
   *
   *  jumps are apply nodes attributed with label symbol, the arguments
   *  will get assigned to the idents.
   *
   *  Note: on 2005-06-09 Martin, Iuli, Burak agreed to have forward
   *        jumps within a Block.
   */
  case class LabelDef(name: Name, params: List[Ident], rhs: Tree)
       extends DefTree with TermTree {
    assert(rhs.isTerm)
  }

  def LabelDef(sym: Symbol, params: List[Symbol], rhs: Tree): LabelDef =
    posAssigner.atPos(sym.pos) {
      LabelDef(sym.name, params map Ident, rhs) setSymbol sym
    }

  /** Import clause
   *
   *  @param expr
   *  @param selectors
   */
  case class Import(expr: Tree, selectors: List[Pair[Name, Name]])
       extends SymTree

  /** Attribute application (constructor arguments + name-value pairs) */
  case class Attribute(constr: Tree, elements: List[Tree])
       extends TermTree

  /** Attributed definition */
  case class Attributed(attribute: Tree, definition: Tree)
       extends Tree {
    override def symbol: Symbol = definition.symbol
    override def symbol_=(sym: Symbol): unit = { definition.symbol = sym }
  }

  /** Documented definition, eliminated by analyzer */
  case class DocDef(comment: String, definition: Tree)
       extends Tree {
    override def symbol: Symbol = definition.symbol
    override def symbol_=(sym: Symbol): unit = { definition.symbol = sym }
  }

  /** Instantiation template
   *
   *  @param parents
   *  @param body
   */
  case class Template(parents: List[Tree], body: List[Tree])
       extends SymTree {
    // System.err.println("TEMPLATE: " + parents);
  }

  def Template(parents: List[Tree], vparamss: List[List[ValDef]], argss: List[List[Tree]], body: List[Tree]): Template = {
    /** Add constructor to template */
    var vparamss1 =
      vparamss map (.map (vd =>
        ValDef(Modifiers(vd.mods.flags & IMPLICIT | PARAM), vd.name, vd.tpt.duplicate, EmptyTree)));
    if (vparamss1.isEmpty ||
        !vparamss1.head.isEmpty && (vparamss1.head.head.mods.flags & IMPLICIT) != 0)
      vparamss1 = List() :: vparamss1;
    val superRef: Tree = Select(Super(nme.EMPTY.toTypeName, nme.EMPTY.toTypeName), nme.CONSTRUCTOR)
    val superCall = posAssigner.atPos(parents.head.pos) { (superRef /: argss) (Apply) }
    val constr: Tree = DefDef(NoMods, nme.CONSTRUCTOR, List(), vparamss1, TypeTree(), superCall)
    Template(parents, List.flatten(vparamss) ::: constr :: body)
  }

  /** Block of expressions (semicolon separated expressions) */
  case class Block(stats: List[Tree], expr: Tree)
       extends TermTree

  /** Case clause in a pattern match, eliminated by TransMatch
   *  (except for occurences in switch statements)
   */
  case class CaseDef(pat: Tree, guard: Tree, body: Tree)
       extends Tree

  /** casedef shorthand */
  def CaseDef(pat: Tree, body: Tree): CaseDef = CaseDef(pat, EmptyTree, body)

  /** Sequence of patterns (comma separated expressions), eliminated by TransMatch */
  case class Sequence(trees: List[Tree])
       extends TermTree

  /** Alternatives of patterns, eliminated by TransMatch, except for
   *  occurences in encoded Switch stmt (=remaining Match(CaseDef(...))
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

  def Bind(sym: Symbol, body: Tree): Bind =
    Bind(sym.name, body) setSymbol sym

  /** Array of expressions, needs to be translated in backend,
   */
  case class ArrayValue(elemtpt: Tree, elems: List[Tree])
       extends TermTree

  /** Anonymous function, eliminated by analyzer */
  case class Function(vparams: List[ValDef], body: Tree)
       extends TermTree with SymTree

  /** Assignment */
  case class Assign(lhs: Tree, rhs: Tree)
       extends TermTree

  /** Conditional expression */
  case class If(cond: Tree, thenp: Tree, elsep: Tree)
       extends TermTree

  /** Pattern matching expression  (before TransMatch)
   *  Switch statements            (after TransMatch)
   *
   *  after TM, cases will satisfy the following constraints:
   *  - all guards are EmptyTree,
   *  - all patterns will be either Literal(Constant(x:Int)) or Alternative(lit|...|lit)
   *  - except for an "otherwise" branch, which has pattern Ident(nme.WILDCARD)
   */
  case class Match(selector: Tree, cases: List[CaseDef])
       extends TermTree

  /** Return expression */
  case class Return(expr: Tree)
       extends TermTree with SymTree

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
  case class New(tpt: Tree)
       extends TermTree {
    assert(tpt.isType)
  }

  /** Factory method for object creation &lt;new tpt(args_1)...(args_n)&gt; */
  def New(tpt: Tree, argss: List[List[Tree]]): Tree = {
    assert(!argss.isEmpty)
    val superRef: Tree = Select(New(tpt), nme.CONSTRUCTOR)
    (superRef /: argss) (Apply)
  }

  /** Type annotation, eliminated by explicit outer */
  case class Typed(expr: Tree, tpt: Tree)
       extends TermTree

  abstract class GenericApply extends TermTree {
    val fun: Tree
    val args: List[Tree]
  }

  /** Type application */
  case class TypeApply(fun: Tree, args: List[Tree])
       extends GenericApply {
    override def symbol: Symbol = fun.symbol
    override def symbol_=(sym: Symbol): unit = { fun.symbol = sym }
  }

  /** Value application */
  case class Apply(fun: Tree, args: List[Tree])
       extends GenericApply {
    override def symbol: Symbol = fun.symbol
    override def symbol_=(sym: Symbol): unit = { fun.symbol = sym }
  }

  /** Super reference */
  case class Super(qual: Name, mix: Name)
       extends TermTree with SymTree

  def Super(sym: Symbol, mix: Name): Tree = Super(sym.name, mix) setSymbol sym

  /** Self reference */
  case class This(qual: Name)
        extends TermTree with SymTree

  def This(sym: Symbol): Tree = This(sym.name) setSymbol sym

  /** Designator */
  case class Select(qualifier: Tree, selector: Name)
       extends SymTree {
    override def isTerm = selector.isTermName
    override def isType = selector.isTypeName
  }

  def Select(qualifier: Tree, sym: Symbol): Select =
    Select(qualifier, sym.name) setSymbol sym

  /** Identifier */
  case class Ident(name: Name)
       extends SymTree {
    override def isTerm = name.isTermName
    override def isType = name.isTypeName
  }

  def Ident(sym: Symbol): Ident =
    Ident(sym.name) setSymbol sym

  /** Literal */
  case class Literal(value: Constant)
        extends TermTree {
    assert(value != null)
  }

  def Literal(value: Any): Literal =
    Literal(Constant(value))

  /** General type term, introduced by RefCheck. */
  case class TypeTree() extends TypTree {
    var original: Tree = _

    def setOriginal(tree: Tree): this.type = {
      original = tree
      setPos(tree.pos)
    }
    override def isEmpty = tpe == null || tpe == NoType
  }

  def TypeTree(tp: Type): TypeTree = TypeTree() setType tp
  // def TypeTree(tp: Type, tree : Tree): TypeTree = TypeTree(tree) setType tp;


  /** Singleton type, eliminated by RefCheck */
  case class SingletonTypeTree(ref: Tree)
        extends TypTree

  /** Type selection, eliminated by RefCheck */
  case class SelectFromTypeTree(qualifier: Tree, selector: Name)
       extends TypTree with SymTree

  /** Intersection type, eliminated by RefCheck */
  case class CompoundTypeTree(templ: Template)
       extends TypTree

  /** Applied type, eliminated by RefCheck */
  case class AppliedTypeTree(tpt: Tree, args: List[Tree])
       extends TypTree {
    override def symbol: Symbol = tpt.symbol
    override def symbol_=(sym: Symbol): unit = { tpt.symbol = sym }
 }

/* A standard pattern match
  case EmptyTree =>
  case PackageDef(name, stats) =>
  case ClassDef(mods, name, tparams, tpt, impl) =>
  case ModuleDef(mods, name, impl) =>                         (eliminated by refcheck)
  case ValDef(mods, name, tpt, rhs) =>
  case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
  case AbsTypeDef(mods, name, lo, hi) =>                          (eliminated by erasure)
  case AliasTypeDef(mods, name, tparams, rhs) =>                  (eliminated by erasure)
  case LabelDef(name, params, rhs) =>
  case Import(expr, selectors) =>                                 (eliminated by typecheck)
  case Attribute(constr, elements) =>                             (eliminated by typecheck)
  case Attributed(attribute, definition) =>                       (eliminated by typecheck)
  case DocDef(comment, definition) =>                             (eliminated by typecheck)
  case Template(parents, body) =>
  case Block(stats, expr) =>
  case CaseDef(pat, guard, body) =>                               (eliminated by transmatch)
  case Sequence(trees) =>                                         (eliminated by transmatch)
  case Alternative(trees) =>                                      (eliminated by transmatch)
  case Star(elem) =>                                              (eliminated by transmatch)
  case Bind(name, body) =>                                        (eliminated by transmatch)
  case ArrayValue(elemtpt, trees) =>                              (introduced by uncurry)
  case Function(vparams, body) =>                                 (eliminated by lambdaLift)
  case Assign(lhs, rhs) =>
  case If(cond, thenp, elsep) =>
  case Match(selector, cases) =>
  case Return(expr) =>
  case Try(block, catches, finalizer) =>
  case Throw(expr) =>
  case New(tpt) =>
  case Typed(expr, tpt) =>                                        (eliminated by erasure)
  case TypeApply(fun, args) =>
  case Apply(fun, args) =>
  case Super(qual, mix) =>
  case This(qual) =>
  case Select(qualifier, selector) =>
  case Ident(name) =>
  case Literal(value) =>
  case TypeTree() =>
  case SingletonTypeTree(ref) =>                                  (eliminated by typecheck)
  case SelectFromTypeTree(qualifier, selector) =>                 (eliminated by typecheck)
  case CompoundTypeTree(templ: Template) =>                       (eliminated by typecheck)
  case AppliedTypeTree(tpt, args) =>                              (eliminated by typecheck)
*/

  abstract class TreeCopier {
    def ClassDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[AbsTypeDef], tpt: Tree, impl: Template): ClassDef
    def PackageDef(tree: Tree, name: Name, stats: List[Tree]): PackageDef
    def ModuleDef(tree: Tree, mods: Modifiers, name: Name, impl: Template): ModuleDef
    def ValDef(tree: Tree, mods: Modifiers, name: Name, tpt: Tree, rhs: Tree): ValDef
    def DefDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[AbsTypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree): DefDef
    def AbsTypeDef(tree: Tree, mods: Modifiers, name: Name, lo: Tree, hi: Tree): AbsTypeDef
    def AliasTypeDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[AbsTypeDef], rhs: Tree): AliasTypeDef
    def LabelDef(tree: Tree, name: Name, params: List[Ident], rhs: Tree): LabelDef
    def Import(tree: Tree, expr: Tree, selectors: List[Pair[Name, Name]]): Import
    def Attribute(tree: Tree, constr: Tree, elements: List[Tree]): Attribute
    def Attributed(tree: Tree, attribute: Tree, definition: Tree): Attributed
    def DocDef(tree: Tree, comment: String, definition: Tree): DocDef
    def Template(tree: Tree, parents: List[Tree], body: List[Tree]): Template
    def Block(tree: Tree, stats: List[Tree], expr: Tree): Block
    def CaseDef(tree: Tree, pat: Tree, guard: Tree, body: Tree): CaseDef
    def Sequence(tree: Tree, trees: List[Tree]): Sequence
    def Alternative(tree: Tree, trees: List[Tree]): Alternative
    def Star(tree: Tree, elem: Tree): Star
    def Bind(tree: Tree, name: Name, body: Tree): Bind
    def ArrayValue(tree: Tree, elemtpt: Tree, trees: List[Tree]): ArrayValue
    def Function(tree: Tree, vparams: List[ValDef], body: Tree): Function
    def Assign(tree: Tree, lhs: Tree, rhs: Tree): Assign
    def If(tree: Tree, cond: Tree, thenp: Tree, elsep: Tree): If
    def Match(tree: Tree, selector: Tree, cases: List[CaseDef]): Match
    def Return(tree: Tree, expr: Tree): Return
    def Try(tree: Tree, block: Tree, catches: List[CaseDef], finalizer: Tree): Try
    def Throw(tree: Tree, expr: Tree): Throw
    def New(tree: Tree, tpt: Tree): New
    def Typed(tree: Tree, expr: Tree, tpt: Tree): Typed
    def TypeApply(tree: Tree, fun: Tree, args: List[Tree]): TypeApply
    def Apply(tree: Tree, fun: Tree, args: List[Tree]): Apply
    def Super(tree: Tree, qual: Name, mix: Name): Super
    def This(tree: Tree, qual: Name): This
    def Select(tree: Tree, qualifier: Tree, selector: Name): Select
    def Ident(tree: Tree, name: Name): Ident
    def Literal(tree: Tree, value: Constant): Literal
    def TypeTree(tree: Tree): TypeTree
    def SingletonTypeTree(tree: Tree, ref: Tree): SingletonTypeTree
    def SelectFromTypeTree(tree: Tree, qualifier: Tree, selector: Name): SelectFromTypeTree
    def CompoundTypeTree(tree: Tree, templ: Template): CompoundTypeTree
    def AppliedTypeTree(tree: Tree, tpt: Tree, args: List[Tree]): AppliedTypeTree
  }

  class StrictTreeCopier extends TreeCopier {
    def ClassDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[AbsTypeDef], tpt: Tree, impl: Template) =
      new ClassDef(mods, name, tparams, tpt, impl).copyAttrs(tree);
    def PackageDef(tree: Tree, name: Name, stats: List[Tree]) =
      new PackageDef(name, stats).copyAttrs(tree)
    def ModuleDef(tree: Tree, mods: Modifiers, name: Name, impl: Template) =
      new ModuleDef(mods, name, impl).copyAttrs(tree)
    def ValDef(tree: Tree, mods: Modifiers, name: Name, tpt: Tree, rhs: Tree) =
      new ValDef(mods, name, tpt, rhs).copyAttrs(tree)
    def DefDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[AbsTypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree) =
      new DefDef(mods, name, tparams, vparamss, tpt, rhs).copyAttrs(tree)
    def AbsTypeDef(tree: Tree, mods: Modifiers, name: Name, lo: Tree, hi: Tree) =
      new AbsTypeDef(mods, name, lo, hi).copyAttrs(tree)
    def AliasTypeDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[AbsTypeDef], rhs: Tree) =
      new AliasTypeDef(mods, name, tparams, rhs).copyAttrs(tree)
    def LabelDef(tree: Tree, name: Name, params: List[Ident], rhs: Tree) =
      new LabelDef(name, params, rhs).copyAttrs(tree)
    def Import(tree: Tree, expr: Tree, selectors: List[Pair[Name, Name]]) =
      new Import(expr, selectors).copyAttrs(tree)
    def Attribute(tree: Tree, constr: Tree, elements: List[Tree]) =
      new Attribute(constr, elements)
    def Attributed(tree: Tree, attribute: Tree, definition: Tree) =
      new Attributed(attribute, definition).copyAttrs(tree)
    def DocDef(tree: Tree, comment: String, definition: Tree) =
      new DocDef(comment, definition).copyAttrs(tree)
    def Template(tree: Tree, parents: List[Tree], body: List[Tree]) =
      new Template(parents, body).copyAttrs(tree)
    def Block(tree: Tree, stats: List[Tree], expr: Tree) =
      new Block(stats, expr).copyAttrs(tree)
    def CaseDef(tree: Tree, pat: Tree, guard: Tree, body: Tree) =
      new CaseDef(pat, guard, body).copyAttrs(tree)
    def Sequence(tree: Tree, trees: List[Tree]) =
      new Sequence(trees).copyAttrs(tree)
    def Alternative(tree: Tree, trees: List[Tree]) =
      new Alternative(trees).copyAttrs(tree)
    def Star(tree: Tree, elem: Tree) =
      new Star(elem).copyAttrs(tree)
    def Bind(tree: Tree, name: Name, body: Tree) =
      new Bind(name, body).copyAttrs(tree)
    def ArrayValue(tree: Tree, elemtpt: Tree, trees: List[Tree]) =
      new ArrayValue(elemtpt, trees).copyAttrs(tree)
    def Function(tree: Tree, vparams: List[ValDef], body: Tree) =
      new Function(vparams, body).copyAttrs(tree)
    def Assign(tree: Tree, lhs: Tree, rhs: Tree) =
      new Assign(lhs, rhs).copyAttrs(tree)
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
      new Apply(fun, args).copyAttrs(tree)
    def Super(tree: Tree, qual: Name, mix: Name) =
      new Super(qual, mix).copyAttrs(tree)
    def This(tree: Tree, qual: Name) =
      new This(qual).copyAttrs(tree)
    def Select(tree: Tree, qualifier: Tree, selector: Name) =
      new Select(qualifier, selector).copyAttrs(tree)
    def Ident(tree: Tree, name: Name) =
      new Ident(name).copyAttrs(tree)
    def Literal(tree: Tree, value: Constant) =
      new Literal(value).copyAttrs(tree)
    def TypeTree(tree: Tree) =
      new TypeTree().copyAttrs(tree)
    def SingletonTypeTree(tree: Tree, ref: Tree) =
      new SingletonTypeTree(ref).copyAttrs(tree)
    def SelectFromTypeTree(tree: Tree, qualifier: Tree, selector: Name) =
      new SelectFromTypeTree(qualifier, selector).copyAttrs(tree)
    def CompoundTypeTree(tree: Tree, templ: Template) =
      new CompoundTypeTree(templ).copyAttrs(tree)
    def AppliedTypeTree(tree: Tree, tpt: Tree, args: List[Tree]) =
      new AppliedTypeTree(tpt, args).copyAttrs(tree)
  }

  class LazyTreeCopier(copy: TreeCopier) extends TreeCopier {
    def this() = this(new StrictTreeCopier);
    def ClassDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[AbsTypeDef], tpt: Tree, impl: Template) = tree match {
      case t @ ClassDef(mods0, name0, tparams0, tpt0, impl0)
      if (mods0 == mods && (name0 == name) && (tparams0 == tparams) && (tpt0 == tpt) && (impl0 == impl)) => t
      case _ => copy.ClassDef(tree, mods, name, tparams, tpt, impl)
    }
    def PackageDef(tree: Tree, name: Name, stats: List[Tree]) = tree match {
      case t @ PackageDef(name0, stats0)
      if ((name0 == name) && (stats0 == stats)) => t
      case _ => copy.PackageDef(tree, name, stats)
    }
    def ModuleDef(tree: Tree, mods: Modifiers, name: Name, impl: Template) = tree match {
      case t @ ModuleDef(mods0, name0, impl0)
      if (mods0 == mods && (name0 == name) && (impl0 == impl)) => t
      case _ => copy.ModuleDef(tree, mods, name, impl)
    }
    def ValDef(tree: Tree, mods: Modifiers, name: Name, tpt: Tree, rhs: Tree) = tree match {
      case t @ ValDef(mods0, name0, tpt0, rhs0)
      if (mods0 == mods && (name0 == name) && (tpt0 == tpt) && (rhs0 == rhs)) => t
      case _ => copy.ValDef(tree, mods, name, tpt, rhs)
    }
    def DefDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[AbsTypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree) = tree match {
      case t @ DefDef(mods0, name0, tparams0, vparamss0, tpt0, rhs0)
      if (mods0 == mods && (name0 == name) && (tparams0 == tparams) && (vparamss0 == vparamss) && (tpt0 == tpt) && (rhs == rhs0)) => t
      case _ => copy.DefDef(tree, mods, name, tparams, vparamss, tpt, rhs)
    }
    def AbsTypeDef(tree: Tree, mods: Modifiers, name: Name, lo: Tree, hi: Tree) = tree match {
      case t @ AbsTypeDef(mods0, name0, lo0, hi0)
      if (mods0 == mods && (name0 == name) && (lo0 == lo) && (hi0 == hi)) => t
      case _ => copy.AbsTypeDef(tree, mods, name, lo, hi)
    }
    def AliasTypeDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[AbsTypeDef], rhs: Tree) = tree match {
      case t @ AliasTypeDef(mods0, name0, tparams0, rhs0)
      if (mods0 == mods && (name0 == name) && (tparams0 == tparams) && (rhs0 == rhs)) => t
      case _ => copy.AliasTypeDef(tree, mods, name, tparams, rhs)
    }
    def LabelDef(tree: Tree, name: Name, params: List[Ident], rhs: Tree) = tree match {
      case t @ LabelDef(name0, params0, rhs0)
      if ((name0 == name) && (params0 == params) && (rhs0 == rhs)) => t
      case _ => copy.LabelDef(tree, name, params, rhs)
    }
    def Import(tree: Tree, expr: Tree, selectors: List[Pair[Name, Name]]) = tree match {
      case t @ Import(expr0, selectors0)
      if ((expr0 == expr) && (selectors0 == selectors)) => t
      case _ => copy.Import(tree, expr, selectors)
    }
    def Attribute(tree: Tree, constr: Tree, elements: List[Tree]) = tree match {
      case t @ Attribute(constr0, elements0)
      if ((constr0 == constr) && (elements0 == elements)) => t
      case _ => copy.Attribute(tree, constr, elements)
    }
    def Attributed(tree: Tree, attribute: Tree, definition: Tree) = tree match {
      case t @ Attributed(attribute0, definition0)
      if ((attribute0 == attribute) && (definition0 == definition)) => t
      case _ => copy.Attributed(tree, attribute, definition)
    }
    def DocDef(tree: Tree, comment: String, definition: Tree) = tree match {
      case t @ DocDef(comment0, definition0)
      if ((comment0 == comment) && (definition0 == definition)) => t
      case _ => copy.DocDef(tree, comment, definition)
    }
    def Template(tree: Tree, parents: List[Tree], body: List[Tree]) = tree match {
      case t @ Template(parents0, body0)
      if ((parents0 == parents) && (body0 == body)) => t
      case _ => copy.Template(tree, parents, body)
    }
    def Block(tree: Tree, stats: List[Tree], expr: Tree) = tree match {
      case t @ Block(stats0, expr0)
      if ((stats0 == stats) && (expr0 == expr)) => t
      case _ => copy.Block(tree, stats, expr)
    }
    def CaseDef(tree: Tree, pat: Tree, guard: Tree, body: Tree) = tree match {
      case t @ CaseDef(pat0, guard0, body0)
      if ((pat0 == pat) && (guard0 == guard) && (body0 == body)) => t
      case _ => copy.CaseDef(tree, pat, guard, body)
    }
    def Sequence(tree: Tree, trees: List[Tree]) = tree match {
      case t @ Sequence(trees0)
      if (trees0 == trees) => t
      case _ => copy.Sequence(tree, trees)
    }
    def Alternative(tree: Tree, trees: List[Tree]) = tree match {
      case t @ Alternative(trees0)
      if (trees0 == trees) => t
      case _ => copy.Alternative(tree, trees)
    }
    def Star(tree: Tree, elem: Tree) = tree match {
      case t @ Star(elem0)
      if (elem0 == elem) => t
      case _ => copy.Star(tree, elem)
    }
    def Bind(tree: Tree, name: Name, body: Tree) = tree match {
      case t @ Bind(name0, body0)
      if ((name0 == name) && (body0 == body)) => t
      case _ => copy.Bind(tree, name, body)
    }
    def ArrayValue(tree: Tree, elemtpt: Tree, trees: List[Tree]) = tree match {
      case t @ ArrayValue(elemtpt0, trees0)
      if ((elemtpt0 == elemtpt) && (trees0 == trees)) => t
      case _ => copy.ArrayValue(tree, elemtpt, trees)
    }
    def Function(tree: Tree, vparams: List[ValDef], body: Tree) = tree match {
      case t @ Function(vparams0, body0)
      if ((vparams0 == vparams) && (body0 == body)) => t
      case _ => copy.Function(tree, vparams, body)
    }
    def Assign(tree: Tree, lhs: Tree, rhs: Tree) = tree match {
      case t @ Assign(lhs0, rhs0)
      if ((lhs0 == lhs) && (rhs0 == rhs)) => t
      case _ => copy.Assign(tree, lhs, rhs)
    }
    def If(tree: Tree, cond: Tree, thenp: Tree, elsep: Tree) = tree match {
      case t @ If(cond0, thenp0, elsep0)
      if ((cond0 == cond) && (thenp0 == thenp) && (elsep0 == elsep)) => t
      case _ => copy.If(tree, cond, thenp, elsep)
    }
    def Match(tree: Tree, selector: Tree, cases: List[CaseDef]) =  tree match {
      case t @ Match(selector0, cases0)
      if ((selector0 == selector) && (cases0 == cases)) => t
      case _ => copy.Match(tree, selector, cases)
    }
    def Return(tree: Tree, expr: Tree) = tree match {
      case t @ Return(expr0)
      if (expr0 == expr) => t
      case _ => copy.Return(tree, expr)
    }
    def Try(tree: Tree, block: Tree, catches: List[CaseDef], finalizer: Tree) = tree match {
      case t @ Try(block0, catches0, finalizer0)
      if ((block0 == block) && (catches0 == catches) && (finalizer0 == finalizer)) => t
      case _ => copy.Try(tree, block, catches, finalizer)
    }
    def Throw(tree: Tree, expr: Tree) = tree match {
      case t @ Throw(expr0)
      if (expr0 == expr) => t
      case _ => copy.Throw(tree, expr)
    }
    def New(tree: Tree, tpt: Tree) = tree match {
      case t @ New(tpt0)
      if (tpt0 == tpt) => t
      case _ => copy.New(tree, tpt)
    }
    def Typed(tree: Tree, expr: Tree, tpt: Tree) = tree match {
      case t @ Typed(expr0, tpt0)
      if ((expr0 == expr) && (tpt0 == tpt)) => t
      case _ => copy.Typed(tree, expr, tpt)
    }
    def TypeApply(tree: Tree, fun: Tree, args: List[Tree]) = tree match {
      case t @ TypeApply(fun0, args0)
      if ((fun0 == fun) && (args0 == args)) => t
      case _ => copy.TypeApply(tree, fun, args)
    }
    def Apply(tree: Tree, fun: Tree, args: List[Tree]) = tree match {
      case t @ Apply(fun0, args0)
      if ((fun0 == fun) && (args0 == args)) => t
      case _ => copy.Apply(tree, fun, args)
    }
    def Super(tree: Tree, qual: Name, mix: Name) = tree match {
      case t @ Super(qual0, mix0)
      if ((qual0 == qual) && (mix0 == mix)) => t
      case _ => copy.Super(tree, qual, mix)
    }
    def This(tree: Tree, qual: Name) = tree match {
      case t @ This(qual0)
      if (qual0 == qual) => t
      case _ => copy.This(tree, qual)
    }
    def Select(tree: Tree, qualifier: Tree, selector: Name) = tree match {
      case t @ Select(qualifier0, selector0)
      if ((qualifier0 == qualifier) && (selector0 == selector)) => t
      case _ => copy.Select(tree, qualifier, selector)
    }
    def Ident(tree: Tree, name: Name) = tree match {
      case t @ Ident(name0)
      if (name0 == name) => t
      case _ => copy.Ident(tree, name)
    }
    def Literal(tree: Tree, value: Constant) = tree match {
      case t @ Literal(value0)
      if (value0 == value) => t
      case _ => copy.Literal(tree, value)
    }
    def TypeTree(tree: Tree) = tree match {
      case t @ TypeTree() => t
      case _ => copy.TypeTree(tree)
    }
    def SingletonTypeTree(tree: Tree, ref: Tree) = tree match {
      case t @ SingletonTypeTree(ref0)
      if (ref0 == ref) => t
      case _ => copy.SingletonTypeTree(tree, ref)
    }
    def SelectFromTypeTree(tree: Tree, qualifier: Tree, selector: Name) = tree match {
      case t @ SelectFromTypeTree(qualifier0, selector0)
      if ((qualifier0 == qualifier) && (selector0 == selector)) => t
      case _ => copy.SelectFromTypeTree(tree, qualifier, selector)
    }
    def CompoundTypeTree(tree: Tree, templ: Template) = tree match {
      case t @ CompoundTypeTree(templ0)
      if (templ0 == templ) => t
      case _ => copy.CompoundTypeTree(tree, templ)
    }
    def AppliedTypeTree(tree: Tree, tpt: Tree, args: List[Tree]) = tree match {
      case t @ AppliedTypeTree(tpt0, args0)
      if ((tpt0 == tpt) && (args0 == args)) => t
      case _ => copy.AppliedTypeTree(tree, tpt, args)
    }
  }

  abstract class Transformer {
    val copy: TreeCopier = new LazyTreeCopier;
    protected var currentOwner: Symbol = definitions.RootClass;
    def transform(tree: Tree): Tree = tree match {
      case EmptyTree =>
        tree
      case PackageDef(name, stats) =>
        atOwner(tree.symbol.moduleClass) {
          copy.PackageDef(tree, name, transformStats(stats, currentOwner))
        }
      case ClassDef(mods, name, tparams, tpt, impl) =>
        atOwner(tree.symbol) {
          copy.ClassDef(tree, mods, name, transformAbsTypeDefs(tparams), transform(tpt), transformTemplate(impl))
        }
      case ModuleDef(mods, name, impl) =>
      atOwner(tree.symbol.moduleClass) {
        copy.ModuleDef(tree, mods, name, transformTemplate(impl))
        }
      case ValDef(mods, name, tpt, rhs) =>
      atOwner(tree.symbol) {
        copy.ValDef(tree, mods, name, transform(tpt), transform(rhs))
        }
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
      atOwner(tree.symbol) {
        copy.DefDef(
            tree, mods, name, transformAbsTypeDefs(tparams), transformValDefss(vparamss), transform(tpt), transform(rhs))
        }
      case AbsTypeDef(mods, name, lo, hi) =>
        atOwner(tree.symbol) {
          copy.AbsTypeDef(tree, mods, name, transform(lo), transform(hi))
        }
      case AliasTypeDef(mods, name, tparams, rhs) =>
        atOwner(tree.symbol) {
          copy.AliasTypeDef(tree, mods, name, transformAbsTypeDefs(tparams), transform(rhs))
        }
      case LabelDef(name, params, rhs) =>
        copy.LabelDef(tree, name, transformIdents(params), transform(rhs)) //bq: Martin, once, atOwner(...) works, also change `LamdaLifter.proxy'
      case Import(expr, selectors) =>
        copy.Import(tree, transform(expr), selectors)
      case Attribute(constr, elements) =>
        copy.Attribute(tree, transform(constr), transformTrees(elements))
      case Attributed(attribute, definition) =>
        copy.Attributed(tree, transform(attribute), transform(definition))
      case DocDef(comment, definition) =>
        copy.DocDef(tree, comment, transform(definition))
      case Template(parents, body) =>
        copy.Template(tree, transformTrees(parents), transformStats(body, tree.symbol))
      case Block(stats, expr) =>
        copy.Block(tree, transformStats(stats, currentOwner), transform(expr))
      case CaseDef(pat, guard, body) =>
        copy.CaseDef(tree, transform(pat), transform(guard), transform(body))
      case Sequence(trees) =>
        copy.Sequence(tree, transformTrees(trees))
      case Alternative(trees) =>
        copy.Alternative(tree, transformTrees(trees))
      case Star(elem) =>
        copy.Star(tree, transform(elem))
      case Bind(name, body) =>
        copy.Bind(tree, name, transform(body))
      case ArrayValue(elemtpt, trees) =>
        copy.ArrayValue(tree, transform(elemtpt), transformTrees(trees))
      case Function(vparams, body) =>
        copy.Function(tree, transformValDefs(vparams), transform(body))
      case Assign(lhs, rhs) =>
        copy.Assign(tree, transform(lhs), transform(rhs))
      case If(cond, thenp, elsep) =>
        copy.If(tree, transform(cond), transform(thenp), transform(elsep))
      case Match(selector, cases) =>
        copy.Match(tree, transform(selector), transformCaseDefs(cases))
      case Return(expr) =>
        copy.Return(tree, transform(expr))
      case Try(block, catches, finalizer) =>
        copy.Try(tree, transform(block), transformCaseDefs(catches), transform(finalizer))
      case Throw(expr) =>
        copy.Throw(tree, transform(expr))
      case New(tpt) =>
        copy.New(tree, transform(tpt))
      case Typed(expr, tpt) =>
        copy.Typed(tree, transform(expr), transform(tpt))
      case TypeApply(fun, args) =>
        copy.TypeApply(tree, transform(fun), transformTrees(args))
      case Apply(fun, args) =>
        copy.Apply(tree, transform(fun), transformTrees(args))
      case Super(qual, mix) =>
        copy.Super(tree, qual, mix)
      case This(qual) =>
        copy.This(tree, qual)
      case Select(qualifier, selector) =>
        copy.Select(tree, transform(qualifier), selector)
      case Ident(name) =>
        copy.Ident(tree, name)
      case Literal(value) =>
        copy.Literal(tree, value)
      case TypeTree() =>
        copy.TypeTree(tree)
      case SingletonTypeTree(ref) =>
        copy.SingletonTypeTree(tree, transform(ref))
      case SelectFromTypeTree(qualifier, selector) =>
        copy.SelectFromTypeTree(tree, transform(qualifier), selector)
      case CompoundTypeTree(templ) =>
        copy.CompoundTypeTree(tree, transformTemplate(templ))
      case AppliedTypeTree(tpt, args) =>
        copy.AppliedTypeTree(tree, transform(tpt), transformTrees(args))
    }

    def transformTrees(trees: List[Tree]): List[Tree] =
      List.mapConserve(trees)(transform)
    def transformTemplate(tree: Template): Template =
      transform(tree: Tree).asInstanceOf[Template]
    def transformAbsTypeDefs(trees: List[AbsTypeDef]): List[AbsTypeDef] =
      List.mapConserve(trees)(tree => transform(tree).asInstanceOf[AbsTypeDef])
    def transformValDefs(trees: List[ValDef]): List[ValDef] =
      List.mapConserve(trees)(tree => transform(tree).asInstanceOf[ValDef])
    def transformValDefss(treess: List[List[ValDef]]): List[List[ValDef]] =
      List.mapConserve(treess)(tree => transformValDefs(tree))
    def transformCaseDefs(trees: List[CaseDef]): List[CaseDef] =
      List.mapConserve(trees)(tree => transform(tree).asInstanceOf[CaseDef])
    def transformIdents(trees: List[Ident]): List[Ident] =
      List.mapConserve(trees)(tree => transform(tree).asInstanceOf[Ident])
    def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] =
      List.mapConserve(stats)(stat =>
        if (exprOwner != currentOwner && stat.isTerm) atOwner(exprOwner)(transform(stat))
        else transform(stat)) filter (EmptyTree !=);
    def transformUnit(unit: CompilationUnit): unit = { unit.body = transform(unit.body) }

    def atOwner[A](owner: Symbol)(trans: => A): A = {
      val prevOwner = currentOwner
      currentOwner = owner
      val result = trans
      currentOwner = prevOwner
      result
    }
  }

  class Traverser {
    protected var currentOwner: Symbol = definitions.RootClass;
    def traverse(tree: Tree): unit = tree match {
      case EmptyTree =>
        ;
      case PackageDef(name, stats) =>
        atOwner(tree.symbol.moduleClass) {
          traverseTrees(stats)
        }
      case ClassDef(mods, name, tparams, tpt, impl) =>
        atOwner(tree.symbol) {
          traverseTrees(tparams); traverse(tpt); traverse(impl)
        }
      case ModuleDef(mods, name, impl) =>
        atOwner(tree.symbol.moduleClass) {
          traverse(impl)
        }
      case ValDef(mods, name, tpt, rhs) =>
        atOwner(tree.symbol) {
          traverse(tpt); traverse(rhs)
        }
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        atOwner(tree.symbol) {
          traverseTrees(tparams); traverseTreess(vparamss); traverse(tpt); traverse(rhs)
        }
      case AbsTypeDef(mods, name, lo, hi) =>
        atOwner(tree.symbol) {
          traverse(lo); traverse(hi)
        }
      case AliasTypeDef(mods, name, tparams, rhs) =>
        atOwner(tree.symbol) {
          traverseTrees(tparams); traverse(rhs)
        }
      case LabelDef(name, params, rhs) =>
        traverseTrees(params); traverse(rhs)
      case Import(expr, selectors) =>
        traverse(expr)
      case Attribute(constr, elements) =>
        traverse(constr); traverseTrees(elements)
      case Attributed(attribute, definition) =>
        traverse(attribute); traverse(definition)
      case DocDef(comment, definition) =>
        traverse(definition)
      case Template(parents, body) =>
        traverseTrees(parents); traverseStats(body, tree.symbol)
      case Block(stats, expr) =>
        traverseTrees(stats); traverse(expr)
      case CaseDef(pat, guard, body) =>
        traverse(pat); traverse(guard); traverse(body)
      case Sequence(trees) =>
        traverseTrees(trees)
      case Alternative(trees) =>
        traverseTrees(trees)
      case Star(elem) =>
        traverse(elem)
      case Bind(name, body) =>
        traverse(body)
      case ArrayValue(elemtpt, trees) =>
        traverse(elemtpt); traverseTrees(trees)
      case Function(vparams, body) =>
        traverseTrees(vparams); traverse(body)
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
    }

    def traverseTrees(trees: List[Tree]): unit =
      trees foreach traverse
    def traverseTreess(treess: List[List[Tree]]): unit =
      treess foreach traverseTrees
    def traverseStats(stats: List[Tree], exprOwner: Symbol): unit =
      stats foreach (stat =>
        if (exprOwner != currentOwner && stat.isTerm) atOwner(exprOwner)(traverse(stat))
        else traverse(stat))
    def apply[T <: Tree](tree: T): T = { traverse(tree); tree }

    def atOwner(owner: Symbol)(traverse: => unit): unit = {
      val prevOwner = currentOwner
      currentOwner = owner
      traverse
      currentOwner = prevOwner
    }
  }

  class TreeSubstituter(from: List[Symbol], to: List[Tree]) extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case Ident(_) =>
        def subst(from: List[Symbol], to: List[Tree]): Tree =
          if (from.isEmpty) tree
          else if (tree.symbol == from.head) to.head
          else subst(from.tail, to.tail);
        subst(from, to)
      case _ =>
        super.transform(tree)
    }
  }

  class TreeTypeSubstituter(from: List[Symbol], to: List[Type]) extends Traverser {
    val typeSubst = new SubstTypeMap(from, to)
    override def traverse(tree: Tree): unit = {
      if (tree.tpe != null) tree.tpe = typeSubst(tree.tpe)
      super.traverse(tree)
    }
    override def apply[T <: Tree](tree: T): T = super.apply(tree.duplicate)
  }

  class TreeSymSubstituter(from: List[Symbol], to: List[Symbol]) extends Traverser {
    val symSubst = new SubstSymMap(from, to);
    override def traverse(tree: Tree): unit = {
      def subst(from: List[Symbol], to: List[Symbol]): unit = {
        if (!from.isEmpty)
          if (tree.symbol == from.head) tree setSymbol to.head
          else subst(from.tail, to.tail)
      }
      if (tree.tpe != null) tree.tpe = symSubst(tree.tpe)
      if (tree.hasSymbol) subst(from, to)
      super.traverse(tree)
    }
    override def apply[T <: Tree](tree: T): T = super.apply(tree.duplicate)
  }

  class ChangeOwnerTraverser(val oldowner: Symbol, val newowner: Symbol) extends Traverser {
    override def traverse(tree: Tree): unit = {
      if ((tree.isDef || tree.isInstanceOf[Function]) && tree.symbol != NoSymbol && tree.symbol.owner == oldowner)
        tree.symbol.owner = newowner;
      super.traverse(tree)
    }
  }

  final class TreeList {
    private var trees = List[Tree]()
    def append(t: Tree): TreeList = { trees = t :: trees; this }
    def append(ts: List[Tree]): TreeList = { trees = ts reverse_::: trees; this }
    def toList: List[Tree] = trees.reverse
  }

  object posAssigner extends Traverser {
    private var pos: int = _
    override def traverse(t: Tree): unit =
      if (t != EmptyTree && t.pos == Position.NOPOS) {
        t.setPos(pos)
        super.traverse(t)
      }
    def atPos[T <: Tree](pos: int)(tree: T): T = {
      this.pos = pos
      traverse(tree)
      tree
    }
  }

  /** A traverser which resets symbol and tpe fields of all nodes in a given tree
   *  except for (1) TypeTree nodes, whose .tpe field is kept and
   *  (2) is a .symbol field refers to a symbol which is defined outside the
   *  tree, it is also kept.
   */
  object resetAttrs extends Traverser {
    private val erasedSyms = new HashSet[Symbol](8)
    override def traverse(tree: Tree): unit = tree match {
      case EmptyTree | TypeTree() =>
        ;
      case _: DefTree =>
        erasedSyms.addEntry(tree.symbol)
        tree.symbol = NoSymbol
        tree.tpe = null
        super.traverse(tree)
      case _ =>
        if (tree.hasSymbol && erasedSyms.contains(tree.symbol)) tree.symbol = NoSymbol
        tree.tpe = null
        super.traverse(tree)
    }
  }
}

