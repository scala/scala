/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.ast

import java.io.{PrintWriter, StringWriter}

import scala.collection.mutable.ListBuffer
import scala.tools.nsc.symtab.{Flags, SymbolTable}
import scala.tools.nsc.symtab.Flags._
import scala.tools.nsc.util.{FreshNameCreator, HashSet, Position, NoPosition, SourceFile}


trait Trees {
  self: SymbolTable =>

  //statistics

  var nodeCount = 0

  trait CompilationUnitTrait {
    var body: Tree
    val source: SourceFile
    def fresh : FreshNameCreator
  }

  type CompilationUnit <: CompilationUnitTrait

  // sub-components --------------------------------------------------

  object treePrinters extends {
    val trees: Trees.this.type = Trees.this
  } with TreePrinters

  lazy val treePrinter = treePrinters.create()

  object treeInfo extends {
    val trees: Trees.this.type = Trees.this
  } with TreeInfo

  val copy = new LazyTreeCopier()

  // modifiers --------------------------------------------------------

  /** @param privateWithin   the qualifier for a private (a type name)
   *                         or nme.EMPTY.toTypeName, if none is given.
   *  @param annotations     the annotations for the definition
   *                         (i.e things starting with @)
   */
  case class Modifiers(flags: Long, privateWithin: Name, annotations: List[Annotation]) {
    def isCovariant     = hasFlag(COVARIANT    )  // marked with `+'
    def isContravariant = hasFlag(CONTRAVARIANT)  // marked with `-'
    def isPrivate   = hasFlag(PRIVATE  )
    def isProtected = hasFlag(PROTECTED)
    def isVariable  = hasFlag(MUTABLE  )
    def isArgument  = hasFlag(PARAM    )
    def isAccessor  = hasFlag(ACCESSOR )
    def isOverride  = hasFlag(OVERRIDE )
    def isAbstract  = hasFlag(ABSTRACT )
    def isDeferred  = hasFlag(DEFERRED )
    def isCase      = hasFlag(CASE     )
    def isSealed    = hasFlag(SEALED   )
    def isFinal     = hasFlag(FINAL    )
    def isTrait     = hasFlag(TRAIT | notDEFERRED) // (part of DEVIRTUALIZE)
    def isImplicit  = hasFlag(IMPLICIT )
    def isPublic    = !isPrivate && !isProtected
    def hasFlag(flag: Long) = (flag & flags) != 0
    def & (flag: Long): Modifiers = {
      val flags1 = flags & flag
      if (flags1 == flags) this
      else Modifiers(flags1, privateWithin, annotations)
    }
    def &~ (flag: Long): Modifiers = {
      val flags1 = flags & (~flag)
      if (flags1 == flags) this
      else Modifiers(flags1, privateWithin, annotations)
    }
    def | (flag: Long): Modifiers = {
      val flags1 = flags | flag
      if (flags1 == flags) this
      else Modifiers(flags1, privateWithin, annotations)
    }
    def withAnnotations(annots: List[Annotation]) =
      if (annots.isEmpty) this
      else Modifiers(flags, privateWithin, annotations ::: annots)
  }

  def Modifiers(flags: Long, privateWithin: Name): Modifiers = Modifiers(flags, privateWithin, List())
  def Modifiers(flags: Long): Modifiers = Modifiers(flags, nme.EMPTY.toTypeName)

  val NoMods = Modifiers(0)

  // @M helper method for asserts that check consistency in kinding
  //def kindingIrrelevant(tp: Type) = (tp eq null) || phase.name == "erasure" || phase.erasedTypes

  abstract class Tree {
    {
      import util.Statistics
      if (Statistics.enabled) nodeCount += 1
    }

    private var rawpos: Position = NoPosition

    def pos = rawpos

    private[this] var rawtpe: Type = _

    def tpe = rawtpe
    def tpe_=(t: Type) = rawtpe = t

    def setPos(pos: Position): this.type = {
      rawpos = pos;
      this
    }

    def setType(tp: Type): this.type = {
      /*assert(kindingIrrelevant(tp) || !kindStar || !tp.isHigherKinded,
               tp+" should not be higher-kinded");*/
      tpe = tp
      this
    }

    def symbol: Symbol = null
    def symbol_=(sym: Symbol) {
      throw new Error("symbol_= inapplicable for " + this)
    }
    def setSymbol(sym: Symbol): this.type = { symbol = sym; this }

    def hasSymbol = false
    def isDef = false
    def isTerm = false
    def isType = false
    def isEmpty = false

    def isErroneous = (tpe ne null) && tpe.isErroneous

    /** Apply `f' to each subtree */
    def foreach(f: Tree => Unit) { new ForeachTreeTraverser(f).traverse(this) }

    /** Find all subtrees matching predicate `p' */
    def filter(f: Tree => Boolean): List[Tree] = {
      val ft = new FilterTreeTraverser(f)
      ft.traverse(this)
      ft.hits.toList
    }

    /** Returns optionally first tree (in a preorder traversal) which satisfies predicate `p',
     *  or None if none exists.
     */
    def find(p: Tree => Boolean): Option[Tree] = {
      val ft = new FindTreeTraverser(p)
      ft.traverse(this)
      ft.result
    }

    /** Is there part of this tree which satisfies predicate `p'? */
    def exists(p: Tree => Boolean): Boolean = !find(p).isEmpty

    override def toString(): String = {
      val buffer = new StringWriter()
      val printer = treePrinters.create(new PrintWriter(buffer))
      printer.print(this)
      printer.flush()
      buffer.toString
    }

    override def hashCode(): Int = super.hashCode()

    override def equals(that: Any): Boolean = that match {
      case t: Tree => this eq t
      case _ => false
    }
    def hashCodeStructure: Int = {
      var hc = getClass.hashCode
      def f(what : Any) : Unit = what match {
      case what : Tree => hc += what.hashCodeStructure
      case what : Iterable[_] => what.foreach(f)
      case what : Product => g(what)
      case null =>
      case what => hc += what.hashCode
      }
      def g(what: Product) {
        hc += what.productArity
        var i = 0
        while (i < what.productArity) {
          f(what.productElement(i))
          i += 1
        }
      }
      this match {
        case p : Product => g(p)
        case _ =>
      }
      hc
    }
    def equalsStructure(that : Tree) = equalsStructure0(that){case (t0,t1) => false}
    def equalsStructure0(that: Tree)(f : (Tree,Tree) => Boolean): Boolean = {
      if (this == that) return true
      if (this.getClass != that.getClass) return false
      val this0 = this.asInstanceOf[Product]
      val that0 = that.asInstanceOf[Product]
      assert(this0.productArity == that0.productArity)
      def equals0(thiz: Any, that: Any): Boolean = thiz match {
        case thiz: Tree =>
          f(thiz,that.asInstanceOf[Tree]) || thiz.equalsStructure0(that.asInstanceOf[Tree])(f)
        case thiz: List[_] =>
          val that0 = that.asInstanceOf[List[Any]]
          if (thiz.length != that0.length) false
          else {
            val results0 = for (i <- 0.until(thiz.length).toList)
              yield equals0(thiz(i), that0(i))
            results0.foldLeft(true)((x,y) => x && y)
          }
        case thiz =>
          thiz == that
      }
      val results = for (i <- 0.until(this0.productArity).toList) yield
        equals0(this0.productElement(i), that0.productElement(i))
      val b = results.foldLeft(true)((x,y) => x && y)
      if (b) (this,that) match {
      case (this0 : TypeTree, that0 : TypeTree) if this0.original != null && that0.original != null =>
        this0.original.equalsStructure0(that0.original)(f)
      case _ => true
      } else false
    }

    def duplicate: this.type =
      (duplicator transform this).asInstanceOf[this.type]

    def shallowDuplicate: this.type =
      ((new ShallowDuplicator(this)) transform this).asInstanceOf[this.type]

    def copyAttrs(tree: Tree): this.type = {
      rawpos = tree.rawpos
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

  /** A tree for a type.  Note that not all type trees implement
    * this trait; in particular, Ident's are an exception. */
  trait TypTree extends Tree {
    override def isType = true
  }

// ----- auxiliary objects and methods ------------------------------

  private lazy val duplicator = new Transformer {
    override val copy = new StrictTreeCopier
  }

  private class ShallowDuplicator(orig: Tree) extends Transformer {
    override val copy = new StrictTreeCopier
    override def transform(tree: Tree) =
      if (tree eq orig)
	super.transform(tree)
      else
	tree
  }

  private def syntheticParams(owner: Symbol, mtp: Type): List[List[Symbol]] = {
    var cnt = 0
    def freshName() = { cnt += 1; newTermName("x$" + cnt) }
    def synthetics(mtp: Type): List[List[Symbol]] = mtp match {
      case PolyType(_, restp) =>
        synthetics(restp)
      case MethodType(formals, restp) =>
        (formals map (f => owner.newValueParameter(owner.pos, freshName()).setInfo(f))) ::
          synthetics(restp)
      case _ =>
        List()
    }
    synthetics(mtp)
  }

//  def nextPhase = if (phase.id > globalPhase.id) phase else phase.next;

// ----- tree node alternatives --------------------------------------

  /** The empty tree */
  case object EmptyTree extends TermTree {
    super.tpe_=(NoType)
    override def tpe_=(t: Type) =
      if (t != NoType) throw new Error("tpe_=("+t+") inapplicable for <empty>")
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
    final def hasFlag(mask: Long): Boolean = (mods.flags & mask) != 0
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
  case class ClassDef(mods: Modifiers, name: Name, tparams: List[TypeDef], impl: Template)
       extends ImplDef

  /**
   *  @param sym       the class symbol
   *  @param impl      ...
   *  @return          ...
   */
  def ClassDef(sym: Symbol, impl: Template): ClassDef =
    posAssigner.atPos(sym.pos) {
      ClassDef(Modifiers(sym.flags),
               sym.name,
               sym.typeParams map TypeDef,
               impl) setSymbol sym
    }

  /** Construct class definition with given class symbol, value parameters,
   *  supercall arguments and template body.
   *
   *  @param sym        the class symbol
   *  @param constrMods the modifiers for the class constructor, i.e. as in `class C private (...)'
   *  @param vparamss   the value parameters -- if they have symbols they
   *                    should be owned by `sym'
   *  @param argss      the supercall arguments
   *  @param body       the template statements without primary constructor
   *                    and value parameter fields.
   *  @return          ...
   */
  def ClassDef(sym: Symbol, constrMods: Modifiers, vparamss: List[List[ValDef]], argss: List[List[Tree]], body: List[Tree]): ClassDef =
    ClassDef(sym,
      Template(sym.info.parents map TypeTree,
               if (sym.thisSym == sym || phase.erasedTypes) emptyValDef else ValDef(sym.thisSym),
               constrMods, vparamss, argss, body))

  /** Singleton object definition
   *
   *  @param mods
   *  @param name
   *  @param impl
   */
  case class ModuleDef(mods: Modifiers, name: Name, impl: Template)
       extends ImplDef

  /**
   *  @param sym       the class symbol
   *  @param impl      ...
   *  @return          ...
   */
  def ModuleDef(sym: Symbol, impl: Template): ModuleDef =
    posAssigner.atPos(sym.pos) {
      ModuleDef(Modifiers(sym.flags), sym.name, impl) setSymbol sym
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
    //assert(kindingIrrelevant(tpt.tpe) || !tpt.tpe.isHigherKinded, tpt.tpe) //@M a value definition should never be typed with a higher-kinded type (values must be classified by types with kind *)
    //tpt.kindStar=true //@M turn on consistency checking in Tree
    assert(rhs.isTerm, rhs)
  }

  def ValDef(sym: Symbol, rhs: Tree): ValDef =
    posAssigner.atPos(sym.pos) {
      ValDef(Modifiers(sym.flags), sym.name, TypeTree(sym.tpe), rhs) setSymbol sym
    }

  def ValDef(sym: Symbol): ValDef = ValDef(sym, EmptyTree)

  object emptyValDef
  extends ValDef(Modifiers(PRIVATE), nme.WILDCARD, TypeTree(NoType), EmptyTree) {
    override def isEmpty = true
  }

  /** Method definition
   *
   *  @param mods
   *  @param name
   *  @param tparams
   *  @param vparamss
   *  @param tpt
   *  @param rhs
   */
  case class DefDef(mods: Modifiers, name: Name, tparams: List[TypeDef],
                    vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree)
       extends ValOrDefDef {
    assert(tpt.isType, tpt)
    //assert(kindingIrrelevant(tpt.tpe) || !tpt.tpe.isHigherKinded, tpt.tpe) //@M a method definition should never be typed with a higher-kinded type (values must be classified by types with kind *)
    //tpt.kindStar=true //@M turn on consistency checking in Tree
    assert(rhs.isTerm, rhs)
  }

  def DefDef(sym: Symbol, mods: Modifiers, vparamss: List[List[ValDef]], rhs: Tree): DefDef =
    posAssigner.atPos(sym.pos) {
      assert(sym != NoSymbol)
      DefDef(Modifiers(sym.flags),
             sym.name,
             sym.typeParams map TypeDef,
             vparamss,
             TypeTree(sym.tpe.finalResultType),
             rhs) setSymbol sym
    }

  def DefDef(sym: Symbol, vparamss: List[List[ValDef]], rhs: Tree): DefDef =
    DefDef(sym, Modifiers(sym.flags), vparamss, rhs)

  def DefDef(sym: Symbol, mods: Modifiers, rhs: List[List[Symbol]] => Tree): DefDef = {
    val vparamss = syntheticParams(sym, sym.tpe)
    DefDef(sym, mods, vparamss map (_.map(ValDef)), rhs(vparamss))
  }

  def DefDef(sym: Symbol, rhs: List[List[Symbol]] => Tree): DefDef =
    DefDef(sym, Modifiers(sym.flags), rhs)

  /** Abstract type, type parameter, or type alias */
  case class TypeDef(mods: Modifiers, name: Name, tparams: List[TypeDef], rhs: Tree)
       extends MemberDef {
    def namePos = pos.offset.map(n => n - name.length).getOrElse(-1)
  }

  /** A TypeDef node which defines given `sym' with given tight hand side `rhs'. */
  def TypeDef(sym: Symbol, rhs: Tree): TypeDef =
    posAssigner.atPos(sym.pos) {
      TypeDef(Modifiers(sym.flags), sym.name, sym.typeParams map TypeDef, rhs) setSymbol sym
    }

  /** A TypeDef node which defines abstract type or type parameter for given `sym' */
  def TypeDef(sym: Symbol): TypeDef =
    TypeDef(sym, TypeBoundsTree(TypeTree(sym.info.bounds.lo), TypeTree(sym.info.bounds.hi)))

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
       extends DefTree with TermTree {
    assert(rhs.isTerm)
  }

  /**
   *  @param sym    the class symbol
   *  @param params ...
   *  @param rhs    ...
   *  @return       ...
   */
  def LabelDef(sym: Symbol, params: List[Symbol], rhs: Tree): LabelDef =
    posAssigner.atPos(sym.pos) {
      LabelDef(sym.name, params map Ident, rhs) setSymbol sym
    }

  /** Import clause
   *
   *  @param expr
   *  @param selectors
   */
  case class Import(expr: Tree, selectors: List[(Name, Name)])
       extends SymTree
    // The symbol of an Import is an import symbol @see Symbol.newImport
    // It's used primarily as a marker to check that the import has been typechecked.

  /** Annotation application (constructor arguments + name-value pairs) */
  case class Annotation(constr: Tree, elements: List[Tree])
       extends TermTree

  /** Documented definition, eliminated by analyzer */
  case class DocDef(comment: String, definition: Tree)
       extends Tree {
    override def symbol: Symbol = definition.symbol
    override def symbol_=(sym: Symbol) { definition.symbol = sym }
    // sean: seems to be important to the IDE
    override def isDef = definition.isDef
    override def isTerm = definition.isTerm
    override def isType = definition.isType
  }

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

  /** Generates a template with constructor corresponding to
   *
   *  constrmods (vparams1_) ... (vparams_n) preSuper { presupers }
   *  extends superclass(args_1) ... (args_n) with mixins { self => body }
   *
   *  This gets translated to
   *
   *  extends superclass with mixins { self =>
   *    presupers' // presupers without rhs
   *    vparamss   // abstract fields corresponding to value parameters
   *    def <init>(vparamss) {
   *      presupers
   *      super.<init>(args)
   *    }
   *    body
   *  }
   */
  def Template(parents: List[Tree], self: ValDef, constrMods: Modifiers, vparamss: List[List[ValDef]], argss: List[List[Tree]], body: List[Tree]): Template = {
    /* Add constructor to template */

    // create parameters for <init>
    var vparamss1 =
      vparamss map (vps => vps.map { vd =>
        ValDef(
          Modifiers(vd.mods.flags & IMPLICIT | PARAM) withAnnotations vd.mods.annotations,
          vd.name, vd.tpt.duplicate, EmptyTree).setPos(vd.pos)
       })
    val (edefs, rest) = body span treeInfo.isEarlyDef
    val (evdefs, etdefs) = edefs partition treeInfo.isEarlyValDef
    val (lvdefs, gvdefs) = List.unzip {
      evdefs map {
        case vdef @ ValDef(mods, name, tpt, rhs) =>
          (copy.ValDef(vdef, Modifiers(PRESUPER), name, tpt, rhs),
           copy.ValDef(vdef, mods, name, TypeTree(), EmptyTree))
      }
    }
    val constrs =
      if (constrMods.isTrait) {
        if (body forall treeInfo.isInterfaceMember) List()
        else List(
          DefDef(NoMods, nme.MIXIN_CONSTRUCTOR, List(), List(List()), TypeTree(), Block(lvdefs, Literal(()))))
      } else {
        // convert (implicit ... ) to ()(implicit ... ) if its the only parameter section
        if (vparamss1.isEmpty ||
            !vparamss1.head.isEmpty && (vparamss1.head.head.mods.flags & IMPLICIT) != 0)
          vparamss1 = List() :: vparamss1;
        val superRef: Tree = Select(Super(nme.EMPTY.toTypeName, nme.EMPTY.toTypeName), nme.CONSTRUCTOR)
        val superCall = posAssigner.atPos(parents.head.pos) { (superRef /: argss) (Apply) }
        List(
          DefDef(constrMods, nme.CONSTRUCTOR, List(), vparamss1, TypeTree(), Block(lvdefs ::: List(superCall), Literal(()))))
      }
    Template(parents, self, gvdefs ::: List.flatten(vparamss) ::: constrs ::: etdefs ::: rest)
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

  /** Sequence of patterns (comma separated expressions), eliminated by the
   *  <code>TransMatch</code> phase.
   */
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
       extends DefTree {
    override def isTerm = name.isTermName
    override def isType = name.isTypeName
  }

  def Bind(sym: Symbol, body: Tree): Bind =
    Bind(sym.name, body) setSymbol sym

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
  case class New(tpt: Tree)
       extends TermTree {
    assert(tpt.isType)
  }

  /** Factory method for object creation <code>&lt;new tpt(args_1)...(args_n)&gt;</code>.
   *  A New(t, as) is expanded to:
   *    (new t).<init>(as)
   *
   *  @param tpt   ...
   *  @param argss ...
   *  @return      ...
   */
  def New(tpt: Tree, argss: List[List[Tree]]): Tree = {
    assert(!argss.isEmpty)
    val superRef: Tree = Select(New(tpt), nme.CONSTRUCTOR)
    (superRef /: argss) (Apply)
  }

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

  def Super(sym: Symbol, mix: Name): Tree = Super(sym.name, mix) setSymbol sym

  /** Self reference */
  case class This(qual: Name)
        extends TermTree with SymTree
    // The symbol of a This is the class to which the this refers.
    // For instance in C.this, it would be C.

  def This(sym: Symbol): Tree = This(sym.name) setSymbol sym

  /** Designator <qualifier> . <selector> */
  case class Select(qualifier: Tree, selector: Name)
       extends SymTree {
    override def isTerm = selector.isTermName
    override def isType = selector.isTypeName
  }

  def Select(qualifier: Tree, sym: Symbol): Select =
    Select(qualifier, sym.name) setSymbol sym

  /** Identifier <name> */
  case class Ident(name: Name)
       extends SymTree {
    override def isTerm = name.isTermName
    override def isType = name.isTypeName
  }

  class BackQuotedIdent(name: Name) extends Ident(name)

  def Ident(sym: Symbol): Ident =
    Ident(sym.name) setSymbol sym

  /** Literal */
  case class Literal(value: Constant)
        extends TermTree {
    assert(value ne null)
  }

  def Literal(value: Any): Literal =
    Literal(Constant(value))

  /** A synthetic term holding an arbitrary type.  Not to be confused with
    * with TypTree, the trait for trees that are only used for type trees.
    * TypeTree's are inserted in several places, but most notably in
    * <code>RefCheck</code>, where the arbitrary type trees are all replaced by
    * TypeTree's. */
  case class TypeTree() extends TypTree {
    override def symbol = if (tpe == null) null else tpe.typeSymbol

    def setOriginal(tree: Tree): this.type = {
      setPos(SyntheticPosition(tree))
    }

    def original: Tree = pos match {
      case SyntheticPosition(orig) => orig
      case _ => null
    }

    override def isEmpty = (tpe eq null) || tpe == NoType
  }

  def TypeTree(tp: Type): TypeTree = TypeTree() setType tp
  // def TypeTree(tp: Type, tree : Tree): TypeTree = TypeTree(tree) setType tp

  /** A tree that has an attribute attached to it */
  case class Annotated(annot: Annotation, arg: Tree) extends Tree {
    override def isType = arg.isType
    override def isTerm = arg.isTerm
  }

  /** Singleton type, eliminated by RefCheck */
  case class SingletonTypeTree(ref: Tree)
        extends TypTree

  /** Type selection <qualifier> # <selector>, eliminated by RefCheck */
  case class SelectFromTypeTree(qualifier: Tree, selector: Name)
       extends TypTree with SymTree

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

  case class Parens(args: List[Tree]) extends Tree // only used during parsing

  trait StubTree extends Tree {
    def underlying : AnyRef
    override def equalsStructure0(that: Tree)(f : (Tree,Tree) => Boolean): Boolean = this eq that
  }

/* A standard pattern match
  case EmptyTree =>
  case PackageDef(name, stats) =>
     // package name { stats }
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
  case Annotation(constr, elements) =>
     // @constr(elements) where constr = tp(args),   (an instance of Apply)
     //                         elements = { val x1 = c1, ..., val xn = cn }
  case DocDef(comment, definition) =>                             (eliminated by typecheck)
     // /** comment */ definition
  case Template(parents, self, body) =>
     // extends parents { self => body }
     // if self is missing it is represented as emptyValDef
  case Block(stats, expr) =>
     // { stats; expr }
  case CaseDef(pat, guard, body) =>                               (eliminated by transmatch/explicitouter)
    // case pat if guard => body
  case Sequence(trees) =>                                         (eliminated by transmatch/explicitouter)
    // pat1, ..., pat_n
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

  abstract class TreeCopier {
    def ClassDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], impl: Template): ClassDef
    def PackageDef(tree: Tree, name: Name, stats: List[Tree]): PackageDef
    def ModuleDef(tree: Tree, mods: Modifiers, name: Name, impl: Template): ModuleDef
    def ValDef(tree: Tree, mods: Modifiers, name: Name, tpt: Tree, rhs: Tree): ValDef
    def DefDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree): DefDef
    def TypeDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], rhs: Tree): TypeDef
    def LabelDef(tree: Tree, name: Name, params: List[Ident], rhs: Tree): LabelDef
    def Import(tree: Tree, expr: Tree, selectors: List[(Name, Name)]): Import
    def Annotation(tree: Tree, constr: Tree, elements: List[Tree]): Annotation
    def DocDef(tree: Tree, comment: String, definition: Tree): DocDef
    def Template(tree: Tree, parents: List[Tree], self: ValDef, body: List[Tree]): Template
    def Block(tree: Tree, stats: List[Tree], expr: Tree): Block
    def CaseDef(tree: Tree, pat: Tree, guard: Tree, body: Tree): CaseDef
    def Sequence(tree: Tree, trees: List[Tree]): Sequence
    def Alternative(tree: Tree, trees: List[Tree]): Alternative
    def Star(tree: Tree, elem: Tree): Star
    def Bind(tree: Tree, name: Name, body: Tree): Bind
    def UnApply(tree: Tree, fun: Tree, args: List[Tree]): UnApply
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
    def ApplyDynamic(tree: Tree, qual: Tree, args: List[Tree]): ApplyDynamic
    def Super(tree: Tree, qual: Name, mix: Name): Super
    def This(tree: Tree, qual: Name): This
    def Select(tree: Tree, qualifier: Tree, selector: Name): Select
    def Ident(tree: Tree, name: Name): Ident
    def Literal(tree: Tree, value: Constant): Literal
    def TypeTree(tree: Tree): TypeTree
    def Annotated(tree: Tree, annot: Annotation, arg: Tree): Annotated
    def SingletonTypeTree(tree: Tree, ref: Tree): SingletonTypeTree
    def SelectFromTypeTree(tree: Tree, qualifier: Tree, selector: Name): SelectFromTypeTree
    def CompoundTypeTree(tree: Tree, templ: Template): CompoundTypeTree
    def AppliedTypeTree(tree: Tree, tpt: Tree, args: List[Tree]): AppliedTypeTree
    def TypeBoundsTree(tree: Tree, lo: Tree, hi: Tree): TypeBoundsTree
    def ExistentialTypeTree(tree: Tree, tpt: Tree, whereClauses: List[Tree]): ExistentialTypeTree
  }

  class StrictTreeCopier extends TreeCopier {
    def ClassDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], impl: Template) =
      new ClassDef(mods, name, tparams, impl).copyAttrs(tree);
    def PackageDef(tree: Tree, name: Name, stats: List[Tree]) =
      new PackageDef(name, stats).copyAttrs(tree)
    def ModuleDef(tree: Tree, mods: Modifiers, name: Name, impl: Template) =
      new ModuleDef(mods, name, impl).copyAttrs(tree)
    def ValDef(tree: Tree, mods: Modifiers, name: Name, tpt: Tree, rhs: Tree) =
      new ValDef(mods, name, tpt, rhs).copyAttrs(tree)
    def DefDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree) =
      new DefDef(mods, name, tparams, vparamss, tpt, rhs).copyAttrs(tree)
    def TypeDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], rhs: Tree) =
      new TypeDef(mods, name, tparams, rhs).copyAttrs(tree)
    def LabelDef(tree: Tree, name: Name, params: List[Ident], rhs: Tree) =
      new LabelDef(name, params, rhs).copyAttrs(tree)
    def Import(tree: Tree, expr: Tree, selectors: List[(Name, Name)]) =
      new Import(expr, selectors).copyAttrs(tree)
    def Annotation(tree: Tree, constr: Tree, elements: List[Tree]) =
      new Annotation(constr, elements).copyAttrs(tree)
    def DocDef(tree: Tree, comment: String, definition: Tree) =
      new DocDef(comment, definition).copyAttrs(tree)
    def Template(tree: Tree, parents: List[Tree], self: ValDef, body: List[Tree]) =
      new Template(parents, self, body).copyAttrs(tree)
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
    def UnApply(tree: Tree, fun: Tree, args: List[Tree]) =
      new UnApply(fun, args).copyAttrs(tree)
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
    def ApplyDynamic(tree: Tree, qual: Tree, args: List[Tree]) =
      new ApplyDynamic(qual, args).copyAttrs(tree)
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
    def Annotated(tree: Tree, annot: Annotation, arg: Tree) =
      new Annotated(annot, arg).copyAttrs(tree)
    def SingletonTypeTree(tree: Tree, ref: Tree) =
      new SingletonTypeTree(ref).copyAttrs(tree)
    def SelectFromTypeTree(tree: Tree, qualifier: Tree, selector: Name) =
      new SelectFromTypeTree(qualifier, selector).copyAttrs(tree)
    def CompoundTypeTree(tree: Tree, templ: Template) =
      new CompoundTypeTree(templ).copyAttrs(tree)
    def AppliedTypeTree(tree: Tree, tpt: Tree, args: List[Tree]) =
      new AppliedTypeTree(tpt, args).copyAttrs(tree)
    def TypeBoundsTree(tree: Tree, lo: Tree, hi: Tree) =
      new TypeBoundsTree(lo, hi).copyAttrs(tree)
    def ExistentialTypeTree(tree: Tree, tpt: Tree, whereClauses: List[Tree]) =
      new ExistentialTypeTree(tpt, whereClauses).copyAttrs(tree)
  }

  class LazyTreeCopier(copy: TreeCopier) extends TreeCopier {
    def this() = this(new StrictTreeCopier)
    def ClassDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], impl: Template) = tree match {
      case t @ ClassDef(mods0, name0, tparams0, impl0)
      if (mods0 == mods && (name0 == name) && (tparams0 == tparams) && (impl0 == impl)) => t
      case _ => copy.ClassDef(tree, mods, name, tparams, impl)
    }
    def PackageDef(tree: Tree, name: Name, stats: List[Tree]) = tree match {
      case t @ PackageDef(name0, stats0)
      if (name0 == name) && (stats0 == stats) => t
      case _ => copy.PackageDef(tree, name, stats)
    }
    def ModuleDef(tree: Tree, mods: Modifiers, name: Name, impl: Template) = tree match {
      case t @ ModuleDef(mods0, name0, impl0)
      if (mods0 == mods) && (name0 == name) && (impl0 == impl) => t
      case _ => copy.ModuleDef(tree, mods, name, impl)
    }
    def ValDef(tree: Tree, mods: Modifiers, name: Name, tpt: Tree, rhs: Tree) = tree match {
      case t @ ValDef(mods0, name0, tpt0, rhs0)
      if (mods0 == mods) && (name0 == name) && (tpt0 == tpt) && (rhs0 == rhs) => t
      case _ => copy.ValDef(tree, mods, name, tpt, rhs)
    }
    def DefDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree) = tree match {
      case t @ DefDef(mods0, name0, tparams0, vparamss0, tpt0, rhs0)
      if (mods0 == mods) && (name0 == name) && (tparams0 == tparams) &&
         (vparamss0 == vparamss) && (tpt0 == tpt) && (rhs == rhs0) => t
      case _ => copy.DefDef(tree, mods, name, tparams, vparamss, tpt, rhs)
    }
    def TypeDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], rhs: Tree) = tree match {
      case t @ TypeDef(mods0, name0, tparams0, rhs0)
      if (mods0 == mods) && (name0 == name) && (tparams0 == tparams) && (rhs0 == rhs) => t
      case _ => copy.TypeDef(tree, mods, name, tparams, rhs)
    }
    def LabelDef(tree: Tree, name: Name, params: List[Ident], rhs: Tree) = tree match {
      case t @ LabelDef(name0, params0, rhs0)
      if (name0 == name) && (params0 == params) && (rhs0 == rhs) => t
      case _ => copy.LabelDef(tree, name, params, rhs)
    }
    def Import(tree: Tree, expr: Tree, selectors: List[(Name, Name)]) = tree match {
      case t @ Import(expr0, selectors0)
      if (expr0 == expr) && (selectors0 == selectors) => t
      case _ => copy.Import(tree, expr, selectors)
    }
    def Annotation(tree: Tree, constr: Tree, elements: List[Tree]) = tree match {
      case t @ Annotation(constr0, elements0)
      if (constr0 == constr) && (elements0 == elements) => t
      case _ => copy.Annotation(tree, constr, elements)
    }
    def DocDef(tree: Tree, comment: String, definition: Tree) = tree match {
      case t @ DocDef(comment0, definition0)
      if (comment0 == comment) && (definition0 == definition) => t
      case _ => copy.DocDef(tree, comment, definition)
    }
    def Template(tree: Tree, parents: List[Tree], self: ValDef, body: List[Tree]) = tree match {
      case t @ Template(parents0, self0, body0)
      if (parents0 == parents) && (self0 == self) && (body0 == body) => t
      case _ => copy.Template(tree, parents, self, body)
    }
    def Block(tree: Tree, stats: List[Tree], expr: Tree) = tree match {
      case t @ Block(stats0, expr0)
      if ((stats0 == stats) && (expr0 == expr)) => t
      case _ => copy.Block(tree, stats, expr)
    }
    def CaseDef(tree: Tree, pat: Tree, guard: Tree, body: Tree) = tree match {
      case t @ CaseDef(pat0, guard0, body0)
      if (pat0 == pat) && (guard0 == guard) && (body0 == body) => t
      case _ => copy.CaseDef(tree, pat, guard, body)
    }
    def Sequence(tree: Tree, trees: List[Tree]) = tree match {
      case t @ Sequence(trees0)
      if trees0 == trees => t
      case _ => copy.Sequence(tree, trees)
    }
    def Alternative(tree: Tree, trees: List[Tree]) = tree match {
      case t @ Alternative(trees0)
      if trees0 == trees => t
      case _ => copy.Alternative(tree, trees)
    }
    def Star(tree: Tree, elem: Tree) = tree match {
      case t @ Star(elem0)
      if elem0 == elem => t
      case _ => copy.Star(tree, elem)
    }
    def Bind(tree: Tree, name: Name, body: Tree) = tree match {
      case t @ Bind(name0, body0)
      if (name0 == name) && (body0 == body) => t
      case _ => copy.Bind(tree, name, body)
    }
    def UnApply(tree: Tree, fun: Tree, args: List[Tree]) = tree match {
      case t @ UnApply(fun0, args0)
      if (fun0 == fun) && (args0 == args) => t
      case _ => copy.UnApply(tree, fun, args)
    }
    def ArrayValue(tree: Tree, elemtpt: Tree, trees: List[Tree]) = tree match {
      case t @ ArrayValue(elemtpt0, trees0)
      if (elemtpt0 == elemtpt) && (trees0 == trees) => t
      case _ => copy.ArrayValue(tree, elemtpt, trees)
    }
    def Function(tree: Tree, vparams: List[ValDef], body: Tree) = tree match {
      case t @ Function(vparams0, body0)
      if (vparams0 == vparams) && (body0 == body) => t
      case _ => copy.Function(tree, vparams, body)
    }
    def Assign(tree: Tree, lhs: Tree, rhs: Tree) = tree match {
      case t @ Assign(lhs0, rhs0)
      if (lhs0 == lhs) && (rhs0 == rhs) => t
      case _ => copy.Assign(tree, lhs, rhs)
    }
    def If(tree: Tree, cond: Tree, thenp: Tree, elsep: Tree) = tree match {
      case t @ If(cond0, thenp0, elsep0)
      if (cond0 == cond) && (thenp0 == thenp) && (elsep0 == elsep) => t
      case _ => copy.If(tree, cond, thenp, elsep)
    }
    def Match(tree: Tree, selector: Tree, cases: List[CaseDef]) =  tree match {
      case t @ Match(selector0, cases0)
      if (selector0 == selector) && (cases0 == cases) => t
      case _ => copy.Match(tree, selector, cases)
    }
    def Return(tree: Tree, expr: Tree) = tree match {
      case t @ Return(expr0)
      if expr0 == expr => t
      case _ => copy.Return(tree, expr)
    }
    def Try(tree: Tree, block: Tree, catches: List[CaseDef], finalizer: Tree) = tree match {
      case t @ Try(block0, catches0, finalizer0)
      if (block0 == block) && (catches0 == catches) && (finalizer0 == finalizer) => t
      case _ => copy.Try(tree, block, catches, finalizer)
    }
    def Throw(tree: Tree, expr: Tree) = tree match {
      case t @ Throw(expr0)
      if expr0 == expr => t
      case _ => copy.Throw(tree, expr)
    }
    def New(tree: Tree, tpt: Tree) = tree match {
      case t @ New(tpt0)
      if tpt0 == tpt => t
      case _ => copy.New(tree, tpt)
    }
    def Typed(tree: Tree, expr: Tree, tpt: Tree) = tree match {
      case t @ Typed(expr0, tpt0)
      if (expr0 == expr) && (tpt0 == tpt) => t
      case _ => copy.Typed(tree, expr, tpt)
    }
    def TypeApply(tree: Tree, fun: Tree, args: List[Tree]) = tree match {
      case t @ TypeApply(fun0, args0)
      if (fun0 == fun) && (args0 == args) => t
      case _ => copy.TypeApply(tree, fun, args)
    }
    def Apply(tree: Tree, fun: Tree, args: List[Tree]) = tree match {
      case t @ Apply(fun0, args0)
      if (fun0 == fun) && (args0 == args) => t
      case _ => copy.Apply(tree, fun, args)
    }
    def ApplyDynamic(tree: Tree, qual: Tree, args: List[Tree]) = tree match {
      case t @ ApplyDynamic(qual0, args0)
      if (qual0 == qual) && (args0 == args) => t
      case _ => copy.ApplyDynamic(tree, qual, args)
    }
    def Super(tree: Tree, qual: Name, mix: Name) = tree match {
      case t @ Super(qual0, mix0)
      if (qual0 == qual) && (mix0 == mix) => t
      case _ => copy.Super(tree, qual, mix)
    }
    def This(tree: Tree, qual: Name) = tree match {
      case t @ This(qual0)
      if qual0 == qual => t
      case _ => copy.This(tree, qual)
    }
    def Select(tree: Tree, qualifier: Tree, selector: Name) = tree match {
      case t @ Select(qualifier0, selector0)
      if (qualifier0 == qualifier) && (selector0 == selector) => t
      case _ => copy.Select(tree, qualifier, selector)
    }
    def Ident(tree: Tree, name: Name) = tree match {
      case t @ Ident(name0)
      if name0 == name => t
      case _ => copy.Ident(tree, name)
    }
    def Literal(tree: Tree, value: Constant) = tree match {
      case t @ Literal(value0)
      if value0 == value => t
      case _ => copy.Literal(tree, value)
    }
    def TypeTree(tree: Tree) = tree match {
      case t @ TypeTree() => t
      case _ => copy.TypeTree(tree)
    }
    def Annotated(tree: Tree, annot: Annotation, arg: Tree) = tree match {
      case t @ Annotated(annot0, arg0)
      if (annot0==annot) => t
      case _ => copy.Annotated(tree, annot, arg)
    }
    def SingletonTypeTree(tree: Tree, ref: Tree) = tree match {
      case t @ SingletonTypeTree(ref0)
      if ref0 == ref => t
      case _ => copy.SingletonTypeTree(tree, ref)
    }
    def SelectFromTypeTree(tree: Tree, qualifier: Tree, selector: Name) = tree match {
      case t @ SelectFromTypeTree(qualifier0, selector0)
      if (qualifier0 == qualifier) && (selector0 == selector) => t
      case _ => copy.SelectFromTypeTree(tree, qualifier, selector)
    }
    def CompoundTypeTree(tree: Tree, templ: Template) = tree match {
      case t @ CompoundTypeTree(templ0)
      if templ0 == templ => t
      case _ => copy.CompoundTypeTree(tree, templ)
    }
    def AppliedTypeTree(tree: Tree, tpt: Tree, args: List[Tree]) = tree match {
      case t @ AppliedTypeTree(tpt0, args0)
      if (tpt0 == tpt) && (args0 == args) => t
      case _ => copy.AppliedTypeTree(tree, tpt, args)
    }
    def TypeBoundsTree(tree: Tree, lo: Tree, hi: Tree) = tree match {
      case t @ TypeBoundsTree(lo0, hi0)
      if (lo0 == lo) && (hi0 == hi) => t
      case _ => copy.TypeBoundsTree(tree, lo, hi)
    }
    def ExistentialTypeTree(tree: Tree, tpt: Tree, whereClauses: List[Tree]) = tree match {
      case t @ ExistentialTypeTree(tpt0, whereClauses0)
      if (tpt0 == tpt) && (whereClauses0 == whereClauses) => t
      case _ => copy.ExistentialTypeTree(tree, tpt, whereClauses)
    }
  }

  abstract class Transformer {
    val copy: TreeCopier = new LazyTreeCopier
    protected var currentOwner: Symbol = definitions.RootClass
    protected def currentMethod = currentOwner.enclMethod
    protected def currentClass = currentOwner.enclClass
    protected def currentPackage = currentOwner.toplevelClass.owner
    def transform(tree: Tree): Tree = tree match {
      case EmptyTree =>
        tree
      case PackageDef(name, stats) =>
        atOwner(tree.symbol.moduleClass) {
          copy.PackageDef(tree, name, transformStats(stats, currentOwner))
        }
      case ClassDef(mods, name, tparams, impl) =>
        atOwner(tree.symbol) {
          copy.ClassDef(tree, mods, name, transformTypeDefs(tparams), transformTemplate(impl))
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
            tree, mods, name, transformTypeDefs(tparams), transformValDefss(vparamss), transform(tpt), transform(rhs))
        }
      case TypeDef(mods, name, tparams, rhs) =>
        atOwner(tree.symbol) {
          copy.TypeDef(tree, mods, name, transformTypeDefs(tparams), transform(rhs))
        }
      case LabelDef(name, params, rhs) =>
        copy.LabelDef(tree, name, transformIdents(params), transform(rhs)) //bq: Martin, once, atOwner(...) works, also change `LamdaLifter.proxy'
      case Import(expr, selectors) =>
        copy.Import(tree, transform(expr), selectors)
      case Annotation(constr, elements) =>
        copy.Annotation(tree, transform(constr), transformTrees(elements))
      case DocDef(comment, definition) =>
        copy.DocDef(tree, comment, transform(definition))
      case Template(parents, self, body) =>
        copy.Template(tree, transformTrees(parents), transformValDef(self), transformStats(body, tree.symbol))
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
      case UnApply(fun, args) =>
        copy.UnApply(tree, fun, transformTrees(args)) // bq: see test/.../unapplyContexts2.scala
      case ArrayValue(elemtpt, trees) =>
        copy.ArrayValue(tree, transform(elemtpt), transformTrees(trees))
      case Function(vparams, body) =>
        atOwner(tree.symbol) {
          copy.Function(tree, transformValDefs(vparams), transform(body))
        }
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
      case ApplyDynamic(qual, args) =>
        copy.ApplyDynamic(tree, transform(qual), transformTrees(args))
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
      case Annotated(annot, arg) =>
        copy.Annotated(tree, transform(annot).asInstanceOf[Annotation], transform(arg))
      case SingletonTypeTree(ref) =>
        copy.SingletonTypeTree(tree, transform(ref))
      case SelectFromTypeTree(qualifier, selector) =>
        copy.SelectFromTypeTree(tree, transform(qualifier), selector)
      case CompoundTypeTree(templ) =>
        copy.CompoundTypeTree(tree, transformTemplate(templ))
      case AppliedTypeTree(tpt, args) =>
        copy.AppliedTypeTree(tree, transform(tpt), transformTrees(args))
      case TypeBoundsTree(lo, hi) =>
        copy.TypeBoundsTree(tree, transform(lo), transform(hi))
      case ExistentialTypeTree(tpt, whereClauses) =>
        copy.ExistentialTypeTree(tree, transform(tpt), transformTrees(whereClauses))
      case tree : StubTree =>
        tree.symbol = NoSymbol
        tree.tpe = null
        tree
    }

    def transformTrees(trees: List[Tree]): List[Tree] =
        List.mapConserve(trees)(transform)
    def transformTemplate(tree: Template): Template =
      transform(tree: Tree).asInstanceOf[Template]
    def transformTypeDefs(trees: List[TypeDef]): List[TypeDef] =
      List.mapConserve(trees)(tree => transform(tree).asInstanceOf[TypeDef])
    def transformValDef(tree: ValDef): ValDef =
      if (tree.isEmpty) tree else transform(tree).asInstanceOf[ValDef]
    def transformValDefs(trees: List[ValDef]): List[ValDef] =
      List.mapConserve(trees)(transformValDef)
    def transformValDefss(treess: List[List[ValDef]]): List[List[ValDef]] =
      List.mapConserve(treess)(transformValDefs)
    def transformCaseDefs(trees: List[CaseDef]): List[CaseDef] =
      List.mapConserve(trees)(tree => transform(tree).asInstanceOf[CaseDef])
    def transformIdents(trees: List[Ident]): List[Ident] =
      List.mapConserve(trees)(tree => transform(tree).asInstanceOf[Ident])
    def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] =
      List.mapConserve(stats)(stat =>
        if (exprOwner != currentOwner && stat.isTerm) atOwner(exprOwner)(transform(stat))
        else transform(stat)) filter (EmptyTree !=)
    def transformUnit(unit: CompilationUnit) { unit.body = transform(unit.body) }

    def atOwner[A](owner: Symbol)(trans: => A): A = {
      val prevOwner = currentOwner
      currentOwner = owner
      val result = trans
      currentOwner = prevOwner
      result
    }
  }

  class Traverser {
    protected var currentOwner: Symbol = definitions.RootClass
    def traverse(tree: Tree): Unit =  tree match {
      case EmptyTree =>
        ;
      case PackageDef(name, stats) =>
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
      case Annotation(constr, elements) =>
        traverse(constr); traverseTrees(elements)
      case Annotated(annot, arg) =>
        traverse(annot); traverse(arg)
      case DocDef(comment, definition) =>
        traverse(definition)
      case Template(parents, self, body) =>
        traverseTrees(parents)
        if (!self.isEmpty) traverse(self)
        traverseStats(body, tree.symbol)
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
      case Parens(ts) =>
        traverseTrees(ts)
      case tree : StubTree =>
    }

    def traverseTrees(trees: List[Tree]) {
      trees foreach traverse
    }
    def traverseTreess(treess: List[List[Tree]]) {
      treess foreach traverseTrees
    }
    def traverseStats(stats: List[Tree], exprOwner: Symbol) {
      stats foreach (stat =>
        if (exprOwner != currentOwner && stat.isTerm) atOwner(exprOwner)(traverse(stat))
        else traverse(stat))
    }
    def apply[T <: Tree](tree: T): T = { traverse(tree); tree }

    def atOwner(owner: Symbol)(traverse: => Unit) {
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

  class TreeTypeSubstituter(val from: List[Symbol], to: List[Type]) extends Traverser {
    val typeSubst = new SubstTypeMap(from, to)
    override def traverse(tree: Tree) {
      if (tree.tpe ne null) tree.tpe = typeSubst(tree.tpe)
      super.traverse(tree)
    }
    override def apply[T <: Tree](tree: T): T = super.apply(tree.duplicate)
    override def toString() = "TreeTypeSubstituter("+from+","+to+")"
  }

  lazy val EmptyTreeTypeSubstituter = new TreeTypeSubstituter(List(), List())

  class TreeSymSubstituter(from: List[Symbol], to: List[Symbol]) extends Traverser {
    val symSubst = new SubstSymMap(from, to)
    override def traverse(tree: Tree) {
      def subst(from: List[Symbol], to: List[Symbol]) {
        if (!from.isEmpty)
          if (tree.symbol == from.head) tree setSymbol to.head
          else subst(from.tail, to.tail)
      }
      if (tree.tpe ne null) tree.tpe = symSubst(tree.tpe)
      if (tree.hasSymbol) subst(from, to)
      super.traverse(tree)
    }
    override def apply[T <: Tree](tree: T): T = super.apply(tree.duplicate)
    override def toString() = "TreeSymSubstituter("+from+","+to+")"
  }

  class ChangeOwnerTraverser(val oldowner: Symbol, val newowner: Symbol) extends Traverser {
    override def traverse(tree: Tree) {
      if ((tree.isDef || tree.isInstanceOf[Function]) &&
          tree.symbol != NoSymbol && tree.symbol.owner == oldowner)
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
    var pos: Position = _
    override def traverse(t: Tree) {
      if (t != EmptyTree && t.pos == NoPosition) {
        t.setPos(pos)
        super.traverse(t)
      }
    }
    def atPos[T <: Tree](pos: Position)(tree: T): T = {
      this.pos = pos
      traverse(tree)
      tree
    }
  }

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

  class FindTreeTraverser(p: Tree => Boolean) extends Traverser {
    var result: Option[Tree] = None
    override def traverse(t: Tree) {
      if (result.isEmpty) {
        if (p(t)) result = Some(t)
        super.traverse(t)
      }
    }
  }

  object resetPos extends Traverser {
    override def traverse(t: Tree) {
      if (t != EmptyTree) t.setPos(NoPosition)
      super.traverse(t)
    }
  }


  /** resets symbol and tpe fields in a tree, @see ResetAttrsTraverse
   */
  def resetAttrs[A<:Tree](x:A):A = {new ResetAttrsTraverser().traverse(x); x}

  /** A traverser which resets symbol and tpe fields of all nodes in a given tree
   *  except for (1) TypeTree nodes, whose <code>.tpe</code> field is kept and
   *  (2) if a <code>.symbol</code> field refers to a symbol which is defined
   *  outside the tree, it is also kept.
   *
   *  (bq:) This traverser has mutable state and should be discarded after use
   */
  class ResetAttrsTraverser extends Traverser {
    private val erasedSyms = new HashSet[Symbol](8)
    override def traverse(tree: Tree): Unit = tree match {
      case EmptyTree | TypeTree() =>
        ;
      case Template(parents, self, body) =>
        tree.symbol = NoSymbol
        tree.tpe = null
        for (stat <- body)
          if (stat.isDef) erasedSyms.addEntry(stat.symbol)
        super.traverse(tree)
      case _: DefTree | Function(_, _) =>
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
  /* hook to memoize trees in IDE */
  trait TreeKind {
    def isType : Boolean
    def isTerm : Boolean
    def isDef : Boolean
    def hasSymbol : Boolean
    def isTop : Boolean
  }

  /** A position to be used for synthetic trees that correspond to some original tree
   *  @note Trees with synthetic positions may not contain trees with real positions inside them!
   */
  case class SyntheticPosition(original: Tree) extends Position {
    override def isDefined: Boolean = true
    override def isSynthetic: Boolean = true
    override def offset: Option[Int] = original.pos.offset
    override def source: Option[SourceFile] = original.pos.source
    override def start: Int = original.pos.start
    override def point: Int = original.pos.point
    override def end: Int = original.pos.end
    override def underlying = original.pos.underlying
    override def focus = original.pos.focus
  }
}

