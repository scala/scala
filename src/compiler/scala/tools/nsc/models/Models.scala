/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.models

import scala.tools.nsc.Global
import scala.tools.nsc.util.Position

/** This abstract class..
 *
 *  @author Sean McDirmid
 *  #version 1.0
 */
abstract class Models {
  val global : Global
  import global._

  def acceptPrivate = true

  abstract class Kind {}
  object CONSTRUCTOR extends Kind
  object OBJECT extends Kind
  object CLASS  extends Kind
  object TRAIT  extends Kind
  object DEF    extends Kind
  object VAL    extends Kind
  object VAR    extends Kind
  object ARG    extends Kind
  object TPARAM extends Kind

  def KINDS = CLASS :: TRAIT :: OBJECT :: CONSTRUCTOR :: TPARAM :: VAL :: VAR :: DEF :: Nil

  def labelFor(kind: Kind): String = kind match {
    case OBJECT => "Object"
    case CLASS  => "Class"
    case TRAIT  => "Trait"
    case DEF    => "Def"
    case VAL    => "Val"
    case VAR    => "Var"
    case ARG    => "Arg"
    case TPARAM => "Type"
    case CONSTRUCTOR => "Constructor"
  }
  def stringsFor(mods: Modifiers) = {
    var modString: List[String] = Nil
    if (mods.isPrivate  ) modString = "private"   :: modString
    if (mods.isProtected) modString = "protected" :: modString
    if (mods.isOverride ) modString = "override"  :: modString
    if (mods.isAbstract ) modString = "abstract"  :: modString
    if (mods.isCase     ) modString = "case"      :: modString
    if (mods.isSealed   ) modString = "sealed"    :: modString
    if (mods.isFinal    ) modString = "final"     :: modString
    if (mods.isTrait    ) modString = "mixin"     :: modString
    modString
  }

  def codeFor(kind: Kind) : String = kind match {
    case CONSTRUCTOR => codeFor(DEF)
    case _ => labelFor(kind).toLowerCase()
  }

  def pluralFor(kind: Kind) : String = kind match {
    case CLASS  => "Classes"
    case _ => labelFor(kind) + "s"
  }

  def kindOf(term0: Symbol) = {
    if (term0.isVariable) VAR
    else if (term0.isValueParameter) ARG
    else if (term0.isMethod) {
      if (term0.nameString.equals("this")) CONSTRUCTOR
      else DEF
    }
    else if (term0.isClass) CLASS
    else if (term0.isModule) OBJECT
    else if (term0.isValue) VAL
    else if (term0.isTypeParameter) TPARAM
    else if (term0.isType) TPARAM
    else {
      // System.err.println("UNRECOGNIZED SYMBOL: " + term0 + " " + name);
      null
    }
  }

  abstract class Model;

  // def textFor(tp : AbsTypeDef) : String = tp.toString()
  def textFor(tree: Tree): String = {
    var ret = ""
    if (tree.symbol != NoSymbol) tree.symbol.name.toString()
    if (ret.equals("<init>")) ret = "this"
    tree match {
      case cdef: ClassDef =>
        ret = ret + "[" +
          (for (val tparam <- cdef.tparams) yield textFor(tparam)) + "]";
        cdef.mods
      case vdef: ValOrDefDef =>
        vdef match {
          case ddef: DefDef =>
          ret = ret + "[" +
            (for (val tparam <- ddef.tparams) yield textFor(tparam)) + "]";
          for (val vparams <- ddef.vparamss) {
            ret = ret + "(" +
              (for (val vparam <- vparams) yield textFor(vparam)) + ")";
          }
          case _ =>
        }
        ret = ret + " : " + textFor(vdef.tpt)
      case atd: AbsTypeDef =>
        ret = ret + ((if(atd.hi != null) " <: " + textFor(atd.hi) else "") +
                     (if(atd.lo != null) " >: " + textFor(atd.lo) else ""));
      case _ =>
        ret = ret + tree.toString()
    }
    ret
  }

  def mods1(tree: Tree) = tree match {
    case mdef: MemberDef => mdef.mods
    case _ => NoMods
  }

  abstract class HasTree(val parent: Composite) extends Model with Ordered[HasTree] {
    var tree : Tree = _
    def update(tree0: Tree): Boolean = {
      tree = tree0
      false
    }
    def replacedBy(tree0: Tree) : Boolean = true
    def text: String = textFor(tree)
    var mods0 = NoMods

    def mods = if (mods0 != NoMods) mods0 else mods1(tree)

    override def toString(): String = tree.toString()

    def compare(that : HasTree): Int = {
      val idx = KINDS.indexOf(kind);
      val jdx = KINDS.indexOf(that.kind);
      if (idx != jdx) return idx - jdx;
      val result = tree.symbol.nameString.compare(that.tree.symbol.nameString);
      if (result != 0) result;
      else toString().compare(that.toString())
    }
    def compare [b >: HasTree <% Ordered[b]](that: b): Int = {
      if (that.isInstanceOf[HasTree]) {
        compare(that.asInstanceOf[HasTree]);
      } else -1;
    }

    def kind = kindOf(tree.symbol)

    //override def    add(from: Composite, model: HasTree): Unit = { parent.add(from, model) }
    //override def remove(from: Composite, model: HasTree): Unit = { parent.remove(from, model) }
  }

  class ImportMod(parent0: Composite) extends HasTree(parent0) {
    def treex = tree.asInstanceOf[Import]

    override def replacedBy(tree0: Tree): Boolean =
      if (super.replacedBy(tree0) && tree0.isInstanceOf[Import]) {
        val tree1 = tree0.asInstanceOf[Import]
        tree1.tpe == treex.tpe
      } else
        false
  }

  class PackageMod(parent0: Composite) extends HasTree(parent0) {
    def treex = tree.asInstanceOf[PackageDef]
  }

  trait Composite extends Model {
    import scala.collection.mutable._

    class Members extends HashSet[HasTree]
    // val members = new Members;
    object members extends Members


    def isMember(tree: Tree): Boolean = tree.isInstanceOf[Import] // imports welcome anywhere.

    def member(tree: Tree, members: List[Tree]): Tree = tree

    def update0(members1 : List[Tree]) : Boolean = {
      // System.err.println("update0 " + this + " " + members1);
      if (members1.length == 1 && members1.head.isInstanceOf[PackageDef])
        return update0(members1.head.asInstanceOf[PackageDef].stats)

      val marked = new HashSet[HasTree]
      var updated = false
      for (val mmbr1 <- members1) if (mmbr1.isInstanceOf[PackageDef]) {
        System.err.println("PACKAGE: " + mmbr1.symbol + " " + members1.length)
      } else if (isMember(mmbr1)) {
        val mmbr2 = member(mmbr1, members1)
        if (mmbr2 != null) {
          var found = false
          for (val mmbr <- members) if (!found && mmbr.replacedBy(mmbr2)) {
            //System.err.println("REPLACE: " + mmbr + " with " + mmbr2)
            mmbr.mods0 = mods1(mmbr1)
            found = true
            updated = mmbr.update(mmbr2) || updated
            marked += mmbr
          }
          if (!found) {
            updated = true
            val add = modelFor(mmbr2, this)
            add.update(mmbr2)
            val sz = members.size
            members += (add)
            assert(members.size == sz + 1)
            marked += add
          }
        }
        // System.err.println("update1 " + this + " " + members + " " + marked)
      }
      val sz = members.size;
      members.intersect(marked);
      updated = updated || sz < members.size;
      // check if anything was removed!
      updated
    }
  }
  abstract class MemberMod(parent0: Composite) extends HasTree(parent0) {
    def treex = tree.asInstanceOf[MemberDef]

    def name: Name = { treex.name; }

    override def replacedBy(tree0: Tree): Boolean =
      if (super.replacedBy(tree0) && tree0.isInstanceOf[MemberDef]) {
        val tree1 = tree0.asInstanceOf[MemberDef]
        treex.toString().equals(tree1.toString())
      } else false

    override def update(tree0: Tree): Boolean = {
      val updated = tree == null || (treex.mods != tree0.asInstanceOf[MemberDef].mods)
      super.update(tree0) || updated;
    }
  }

  abstract class MemberComposite(parent0: Composite) extends MemberMod(parent0) with Composite

  trait HasClassObjects extends Composite {
    override def isMember(tree: Tree): Boolean =
      super.isMember(tree) || tree.isInstanceOf[ImplDef]
  }

  abstract class ValOrDefMod(parent0: Composite) extends MemberComposite(parent0) with HasClassObjects {
    def treey = tree.asInstanceOf[ValOrDefDef]
    override def replacedBy(tree0: Tree): Boolean =
      super.replacedBy(tree0) && tree0.isInstanceOf[ValOrDefDef]

    override def update(tree0: Tree): Boolean = {
      val tree1 = tree0.asInstanceOf[ValOrDefDef]
      val updated = tree == null || treex.tpe != tree1.tpe
      update0(flatten(tree1.rhs, (tree2 : Tree) => isMember(tree2)))
      super.update(tree0) || updated
    }
  }

  class ValMod(parent0: Composite) extends ValOrDefMod(parent0) {
    def treez = tree.asInstanceOf[ValDef]
    override def replacedBy(tree0: Tree): Boolean =
      super.replacedBy(tree0) && tree0.isInstanceOf[ValDef]
  }

  class DefMod(parent0: Composite) extends ValOrDefMod(parent0) {
    def treez = tree.asInstanceOf[DefDef]

    override def replacedBy(tree0: Tree) : Boolean =
      if (super.replacedBy(tree0) && tree0.isInstanceOf[DefDef]) {
        val tree1 = tree0.asInstanceOf[DefDef]
        if (tree1.vparamss.length == treez.vparamss.length) {
          val tpz = for (val vd <- treez.vparamss) yield for (val xd <- vd) yield xd.tpe;
          val tp1 = for (val vd <- tree1.vparamss) yield for (val xd <- vd) yield xd.tpe;
          tpz == tp1
        } else false
      } else false
  }

  abstract class ImplMod(parent0: Composite) extends MemberComposite(parent0) with HasClassObjects {
    def treey = tree.asInstanceOf[ImplDef]
    override def replacedBy(tree0: Tree): Boolean =
      super.replacedBy(tree0) && tree0.isInstanceOf[ImplDef]
    override def isMember(tree: Tree): Boolean = (super.isMember(tree) ||
      (tree.isInstanceOf[ValOrDefDef] &&
        (acceptPrivate || !tree.asInstanceOf[ValOrDefDef].mods.isPrivate)
       /* && !tree.asInstanceOf[ValOrDefDef].mods.isPrivate */
       /* && !tree.asInstanceOf[ValOrDefDef].mods.isAccessor */) ||
      tree.isInstanceOf[AliasTypeDef]);

    override def member(tree: Tree, members: List[Tree]): Tree = {
      val tree0 = if (tree.isInstanceOf[DefDef]) {
        val ddef = tree.asInstanceOf[DefDef]
        if (ddef.mods.isAccessor && ddef.symbol != null) {
          val sym = ddef.symbol.accessed
          val ret = for (val member <- members; member.symbol == sym) yield {
            member;
          }
          if (ret.isEmpty) null
          else ret.head
        } else tree
      } else super.member(tree, members)

      def sym = tree0.symbol
      if (tree0 == null || sym.pos == NoPos) null
      else if (!acceptPrivate &&
        tree0.isInstanceOf[ValOrDefDef] && tree0.asInstanceOf[ValOrDefDef].mods.isPrivate) null;
      else tree0;
    }

    def children(tree0: Tree): List[Tree] =
      tree0.asInstanceOf[ImplDef].impl.body

    override def update(tree0: Tree): Boolean = {
      var updated = update0(children(tree0))
      super.update(tree0) || updated
    }
  }

  class  ClassMod(parent0: Composite) extends ImplMod(parent0) {
    def treez = tree.asInstanceOf[ClassDef]
    override def replacedBy(tree0: Tree): Boolean =
      super.replacedBy(tree0) && tree0.isInstanceOf[ClassDef]
  }

  class ObjectMod(parent0: Composite) extends ImplMod(parent0) {
    def treez = tree.asInstanceOf[ModuleDef]
    override def replacedBy(tree0: Tree): Boolean =
      super.replacedBy(tree0) && tree0.isInstanceOf[ModuleDef]
  }
  class AliasTypeMod(parent0: Composite) extends MemberMod(parent0) {
    def treey = tree.asInstanceOf[AliasTypeDef];
    override def replacedBy(tree0 : Tree) : Boolean = (super.replacedBy(tree0) && tree0.isInstanceOf[AliasTypeDef]);
  }

  class AbsTypeMod(parent0: Composite) extends HasTree(parent0) {
    def treey = tree.asInstanceOf[AbsTypeDef]
    override def replacedBy(tree0: Tree): Boolean =
      super.replacedBy(tree0) && tree0.isInstanceOf[AbsTypeDef]
  }
  def SourceMod(original : CompilationUnit) = new SourceMod(original);

  class SourceMod(val original : CompilationUnit) extends Composite with HasClassObjects {
    update(original);
    //var listener : Listener = null;
    def update(unit : CompilationUnit) = unit.body match {
      case pdef : PackageDef => try {
	update0(pdef.stats);
      } catch {
        case e : Error => members.clear; update0(pdef.stats);
      }
      case _ =>
    }


    override def isMember(tree: Tree): Boolean =
      super.isMember(tree) || tree.isInstanceOf[Import]
  }

  def flatten0(exprs: List[Tree], filter: (Tree) => Boolean): List[Tree] =
    for (val expr <- exprs; val t: Tree <- flatten(expr,filter)) yield t;

  def flatten(expr : Tree, filter: (Tree) => Boolean) : List[Tree] =
    if (filter(expr)) expr :: Nil; else expr match {
      case Block(stats, last) =>
        flatten0(stats, filter) ::: flatten(last, filter)
      case If(cond, thenp, elsep) =>
        flatten(cond,filter) ::: flatten(thenp,filter) ::: flatten(elsep,filter);
      case Assign(lhs, rhs) =>
        flatten(rhs, filter)
      case CaseDef(pat, guard, body) =>
        flatten(body, filter)
      case Return(expr0) =>
        flatten(expr0, filter)
      case Throw(expr0) =>
        flatten(expr0,filter)
      case Try(block, catches, finalizer) =>
        flatten(block, filter) ::: flatten(finalizer, filter) ::: flatten0(catches, filter)
      case Match(selector, cases) =>
        flatten(selector, filter) ::: flatten0(cases, filter)
      case Apply(fun, args) =>
        flatten(fun, filter) ::: flatten0(args, filter)
      case TypeApply(fun, args) =>
        flatten(fun, filter) ::: flatten0(args, filter)
      case _ =>
        Nil
    }

  def modelFor(tree: Tree, parent: Composite): HasTree = tree match {
    case _: ValDef       => new ValMod(parent)
    case _: DefDef       => new DefMod(parent)
    case _: ClassDef     => new ClassMod(parent)
    case _: ModuleDef    => new ObjectMod(parent)
    case _: AliasTypeDef => new AliasTypeMod(parent)
    case _: AbsTypeDef   => new AbsTypeMod(parent)
    case _: Import       => new ImportMod(parent)
  }

}
