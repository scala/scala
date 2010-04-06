/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc
package models

import scala.tools.nsc.Global

/** This abstract class ...
 *
 *  @author  Sean McDirmid
 *  @version 1.0
 */
abstract class Models {
  val global: Global
  import global._

  def acceptPrivate = true

  object Kinds extends Enumeration  {
    type Kind = Value
    val CONSTRUCTOR = Value("Constructor")
    val OBJECT      = Value("Object")
    val CLASS       = Value("Class")
    val TRAIT       = Value("Trait")
    val DEF         = Value("Def")
    val VAL         = Value("Val")
    val VAR         = Value("Var")
    val ARG         = Value("Arg")
    val TPARAM      = Value("Type")
  }
  import Kinds._

  def KINDS = List(CLASS, TRAIT, OBJECT, CONSTRUCTOR, TPARAM, VAL, VAR, DEF)

  def labelFor(kind: Kind): String = kind.toString

  def stringsFor(mods: Modifiers) = {
    var modString: List[String] = Nil
    if (mods.isPrivate  ) modString = "private"   :: modString
    if (mods.isProtected) modString = "protected" :: modString
    if (mods.isOverride ) modString = "override"  :: modString
    if (mods.isAbstract ) modString = "abstract"  :: modString
    if (mods.isDeferred ) modString = "abstract"  :: modString
    if (mods.isCase     ) modString = "case"      :: modString
    if (mods.isSealed   ) modString = "sealed"    :: modString
    if (mods.isFinal    ) modString = "final"     :: modString
    if (mods.isImplicit ) modString = "implicit"  :: modString
    modString
  }

  def codeFor(kind: Kind): String = kind match {
    case CONSTRUCTOR => codeFor(DEF)
    case _ => labelFor(kind).toLowerCase()
  }

  def pluralFor(kind: Kind): String = kind match {
    case CLASS  => "Classes"
    case _ => labelFor(kind) + "s"
  }

  def kindOf(tree: Tree) = {
    val term0 = tree.symbol;
    if (term0 != NoSymbol) {
      if (term0.isVariable) VAR
      else if (term0.isValueParameter) ARG
      else if (term0.isMethod) {
        if (term0.nameString.equals("this")) CONSTRUCTOR
        else DEF
      }
      else if (term0.isClass) {
        if (tree.asInstanceOf[MemberDef].mods.isTrait) TRAIT
        else CLASS
      }
      else if (term0.isModule) OBJECT
      else if (term0.isValue) VAL
      else if (term0.isTypeParameter) TPARAM
      else if (term0.isType) TPARAM
      else {
        // Console.err.println("UNRECOGNIZED SYMBOL: " + term0 + " " + name);
        null
      }
    } else {
      val ddef = tree.asInstanceOf[ValOrDefDef];
      if (ddef.mods.hasFlag(symtab.Flags.MUTABLE)) VAR;
      else VAL;
    }
  }

  abstract class Model

  // def textFor(tp : AbsTypeDef) : String = tp.toString()

  /**
   *  @param tree ...
   *  @return     ...
   */
  def textFor(tree: Tree): String = {
    var ret = ""
    if (tree.symbol != NoSymbol) tree.symbol.name.toString()
    if (ret.equals("<init>")) ret = "this"
    tree match {
      case cdef: ClassDef =>
        ret = ret + "[" +
          (for (tparam <- cdef.tparams) yield textFor(tparam)) + "]";
        cdef.mods
      case vdef: ValOrDefDef =>
        vdef match {
          case ddef: DefDef =>
          ret = ret + "[" +
            (for (tparam <- ddef.tparams) yield textFor(tparam)) + "]";
          for (vparams <- ddef.vparamss) {
            ret = ret + "(" +
              (for (vparam <- vparams) yield textFor(vparam)) + ")";
          }
          case _ =>
        }
        ret = ret + " : " + textFor(vdef.tpt)
/* Martin to Sean: Please check whether this can be dropped or does it need to be adapted?
      case atd: AbsTypeDef =>
        ret = ret + "[" + (for (tparam <- atd.tparams) yield textFor(tparam)) + "]" +
                     ((if(atd.hi ne null) " <: " + textFor(atd.hi) else "") +
                     (if(atd.lo ne null) " >: " + textFor(atd.lo) else ""));
*/
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
    def replacedBy(tree0: Tree): Boolean = true
    def text: String = textFor(tree)
    var mods0 = NoMods

    def mods = if (mods0 != NoMods) mods0 else mods1(tree)

    override def toString(): String = tree.toString()

    def compare(that: HasTree): Int = {
      val idx = KINDS.indexOf(kind)
      val jdx = KINDS.indexOf(that.kind)
      if (idx != jdx) return idx - jdx
      val result = tree.symbol.nameString.compare(that.tree.symbol.nameString)
      if (result != 0) result
      else toString().compare(that.toString())
    }
    def compare [b >: HasTree <% Ordered[b]](that: b): Int = {
      if (that.isInstanceOf[HasTree])
        compare(that.asInstanceOf[HasTree])
      else -1
    }

    def kind = kindOf(tree)

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
    // val members = new Members
    object members extends Members

    def isMember(tree: Tree): Boolean = tree.isInstanceOf[Import] // imports welcome anywhere.

    def member(tree: Tree, members: List[Tree]): Tree = tree

    def update0(members1: List[Tree]): Boolean = {
      // Console.err.println("update0 " + this + " " + members1)
      // Martin: This is rather ugly code. We should use pattern matching here!
      if (members1.length == 1 && members1.head.isInstanceOf[PackageDef])
        return update0(members1.head.asInstanceOf[PackageDef].stats)
      val marked = new HashSet[HasTree]
      var updated = false
      for (mmbr1 <- members1) if (mmbr1.isInstanceOf[PackageDef]) {
        Console.err.println("PACKAGE: " + mmbr1.symbol + " " + members1.length)
      } else if (isMember(mmbr1)) {
        val mmbr2 = member(mmbr1, members1)
        if (mmbr2 ne null) {
          var found = false
          for (mmbr <- members) if (!found && mmbr.replacedBy(mmbr2)) {
            //Console.err.println("REPLACE: " + mmbr + " with " + mmbr2)
            mmbr.mods0 = mods1(mmbr1)
            found = true
            updated = mmbr.update(mmbr2) || updated
            marked += mmbr
          }
          if (!found) {
            updated = true
            val add = modelFor(mmbr2, this)
            add.update(mmbr2)
            add.mods0 = mods1(mmbr1) &
              ~symtab.Flags.ACCESSOR & ~symtab.Flags.SYNTHETIC
            val sz = members.size
            members += (add)
            assert(members.size == sz + 1)
            marked += add
          }
        }
        // Console.err.println("update1 " + this + " " + members + " " + marked)
      }
      val sz = members.size
      members.intersect(marked)
      updated = updated || sz < members.size
      // check if anything was removed!
      updated
    }
  }
  abstract class MemberMod(parent0: Composite) extends HasTree(parent0) {
    def treex = tree.asInstanceOf[MemberDef]

    def name: Name = treex.name

    override def replacedBy(tree0: Tree): Boolean =
      if (super.replacedBy(tree0) && tree0.isInstanceOf[MemberDef]) {
        val tree1 = tree0.asInstanceOf[MemberDef]
        treex.toString().equals(tree1.toString())
      } else false

    override def update(tree0: Tree): Boolean = {
      val updated = (tree eq null) || (treex.mods != tree0.asInstanceOf[MemberDef].mods)
      super.update(tree0) || updated;
    }
  }

  abstract class MemberComposite(parent0: Composite) extends MemberMod(parent0) with Composite

  trait HasClassObjects extends Composite {
    override def isMember(tree: Tree): Boolean =
      super.isMember(tree) || tree.isInstanceOf[ImplDef]
  }

  abstract class ValOrDefMod(parent0: Composite) extends MemberComposite(parent0) with HasClassObjects {
    override def replacedBy(tree0: Tree): Boolean =
      super.replacedBy(tree0) && tree0.isInstanceOf[ValOrDefDef]

    override def update(tree0: Tree): Boolean = {
      val tree1 = tree0.asInstanceOf[ValOrDefDef]
      val updated = (tree eq null) || treex.tpe != tree1.tpe
      update0(flatten(tree1.rhs, (tree2: Tree) => isMember(tree2)))
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
          val tpz = for (vd <- treez.vparamss) yield for (xd <- vd) yield xd.tpe;
          val tp1 = for (vd <- tree1.vparamss) yield for (xd <- vd) yield xd.tpe;
          tpz == tp1
        } else false
      } else false
  }

  abstract class ImplMod(parent0: Composite)
  extends MemberComposite(parent0) with HasClassObjects {
    override def replacedBy(tree0: Tree): Boolean =
      super.replacedBy(tree0) && tree0.isInstanceOf[ImplDef]
    override def isMember(tree: Tree): Boolean = (super.isMember(tree) ||
      (tree.isInstanceOf[ValOrDefDef] &&
        (acceptPrivate || !tree.asInstanceOf[ValOrDefDef].mods.isPrivate)
       /* && !tree.asInstanceOf[ValOrDefDef].mods.isPrivate */
       /* && !tree.asInstanceOf[ValOrDefDef].mods.isAccessor */) ||
      treeInfo.isAliasTypeDef(tree))

    override def member(tree: Tree, members: List[Tree]): Tree = {
      val tree0 = if (tree.isInstanceOf[DefDef]) {
        val ddef = tree.asInstanceOf[DefDef]
        ddef.mods
        if (ddef.mods.isAccessor && (ddef.symbol ne null)) {
          val sym0 = ddef.symbol;
          if (sym0.isSetter) return null;
          assert(sym0.isGetter);
          val sym = sym0.accessed
          val ret = if (sym == NoSymbol) {
            val sym = analyzer.underlying(sym0)
            //val name = nme.getterToSetter(sym0.name)
            //val setter = sym0.owner.info.decl(name);
            val isVar = sym.isVariable;
            val mods = (ddef.mods |
              (if (isVar) symtab.Flags.MUTABLE else 0) | symtab.Flags.DEFERRED) &
                ~symtab.Flags.ACCESSOR & ~symtab.Flags.SYNTHETIC
            val tree =
              ValDef(mods, ddef.name, ddef.tpt, ddef.rhs).setPos(ddef.pos).setSymbol(sym);
            tree :: Nil;
          } else for (member <- members if member.symbol == sym) yield member
          if (ret.isEmpty) null
          else ret.head
        } else tree
      } else super.member(tree, members)

      def sym = tree0.symbol
      if ((tree0 eq null) || tree0.pos == NoPosition) null
      else if (!acceptPrivate &&
               tree0.isInstanceOf[ValOrDefDef] &&
               tree.asInstanceOf[ValOrDefDef].mods.isPrivate) null
      else tree0
    }

    def children(tree0: Tree): List[Tree] =
      tree0.asInstanceOf[ImplDef].impl.body

    override def update(tree0: Tree): Boolean = {
      var updated = update0(children(tree0))
      super.update(tree0) || updated
    }
  }

  class ClassMod(parent0: Composite) extends ImplMod(parent0) {
    def treez = tree.asInstanceOf[ClassDef]
    override def replacedBy(tree0: Tree): Boolean =
      super.replacedBy(tree0) && tree0.isInstanceOf[ClassDef]
  }

  class ObjectMod(parent0: Composite) extends ImplMod(parent0) {
    def treez = tree.asInstanceOf[ModuleDef]
    override def replacedBy(tree0: Tree): Boolean =
      super.replacedBy(tree0) && tree0.isInstanceOf[ModuleDef]
  }
  class TypeMod(parent0: Composite) extends MemberMod(parent0) {
    override def replacedBy(tree0 : Tree) : Boolean = (super.replacedBy(tree0) && tree0.isInstanceOf[TypeDef]);
  }
  def SourceMod(original: CompilationUnit) = new SourceMod(original)

  class SourceMod(val original: CompilationUnit) extends Composite with HasClassObjects {
    update(original)
    //var listener : Listener = null;
    def update(unit: CompilationUnit) = unit.body match {
      case pdef: PackageDef => try {
        update0(pdef.stats)
      } catch {
        case e: Error => members.clear; update0(pdef.stats)
      }
      case _ =>
    }

    override def isMember(tree: Tree): Boolean =
      super.isMember(tree) || tree.isInstanceOf[Import]
  }

  def flatten0(exprs: List[Tree], filter: (Tree) => Boolean): List[Tree] =
    for (expr <- exprs; t: Tree <- flatten(expr,filter)) yield t

  def flatten(expr: Tree, filter: (Tree) => Boolean): List[Tree] =
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
    case _: TypeDef      => new TypeMod(parent)
    case _: Import       => new ImportMod(parent)
  }

}
