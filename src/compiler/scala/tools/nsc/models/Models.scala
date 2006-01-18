/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.models;

import scala.tools.nsc.Global;
import scala.tools.nsc.util.Position;

class Models(val global : Global) {
  import global._;

  abstract class Listener {
	    def    add(from : Composite, model : HasTree) : Unit;
	    def remove(from : Composite, model : HasTree) : Unit;

  }
  abstract class Model extends Listener;


  abstract class HasTree(val parent : Composite) extends Model {
    var tree : Tree = _;
    def update(tree0 : Tree): Boolean = {
      tree = tree0;
      false;
    }
    def replacedBy(tree0 : Tree) : Boolean = true;



    override def    add(from : Composite, model : HasTree) : Unit = { parent.add(from, model); }
    override def remove(from : Composite, model : HasTree) : Unit = { parent.remove(from, model); }
  }
  class ImportMod(parent0 : Composite) extends HasTree(parent0) {
    def treex = tree.asInstanceOf[Import];

    override def replacedBy(tree0 : Tree) : Boolean = if (super.replacedBy(tree0) && tree0.isInstanceOf[Import]) {
      val tree1 = tree0.asInstanceOf[Import];
      tree1.tpe == treex.tpe;
    } else false;
  }
  class PackageMod(parent0 : Composite) extends HasTree(parent0) {
	    def treex = tree.asInstanceOf[PackageDef];
	}

  [_trait_] abstract class Composite extends Model {
    import scala.collection.mutable._;


    class Members extends HashSet[HasTree] with ObservableSet[HasTree, Members];
	//    val members = new Members;
    object members extends Members;


    object subscriber extends Subscriber[Message[HasTree] with Undoable, Members] {
    	def notify(pub: Members, event: Message[HasTree] with Undoable): Unit = event match {
				//case Include(elem) =>    add(Composite.this, i.elem);
				//case  Remove(elem) => remove(Composite.this, i.elem);
				case i : Include[HasTree] with Undoable  =>    add(Composite.this, i.elem);
				case r : Remove [HasTree] with Undoable  => remove(Composite.this, r.elem);
			}
    }
    members.subscribe(subscriber);





    def isMember(tree : Tree) : Boolean = tree.isInstanceOf[Import]; // imports welcome anywhere.

    def member(tree: Tree, members : List[Tree]) : Tree = tree;

    def update0(members1 : List[Tree]) : Boolean = {
      // System.err.println("update0 " + this + " " + members1);
      if (members1.length == 1 && members1.head.isInstanceOf[PackageDef])
    	  return update0(members1.head.asInstanceOf[PackageDef].stats);

      val marked = new HashSet[HasTree];
      var updated = false;
      for (val mmbr1 <- members1) if (mmbr1.isInstanceOf[PackageDef]) {
    	  System.err.println("PACKAGE: " + mmbr1.symbol + " " + members1.length);

      } else if (isMember(mmbr1)) {
				val mmbr2 = member(mmbr1, members1);
				if (mmbr2 != null) {
				  var found = false;
				  for (val mmbr <- members) if (!found && mmbr.replacedBy(mmbr2)) {
				    found = true;
				    updated = mmbr.update(mmbr2) || updated;
				    marked += mmbr;
				  }
				  if (!found) {
				    updated = true;
				    val add = modelFor(mmbr2, this);
				    add.update(mmbr2);
				    members += (add);
				    marked += add;
				  }
				}
				// System.err.println("update1 " + this + " " + members + " " + marked);
      }
      val sz = members.size;
      members.intersect(marked);
      updated = updated || sz < members.size;
      // check if anything was removed!
      updated;
    }
  }
  abstract class MemberMod(parent0 : Composite) extends HasTree(parent0) {
    def treex = tree.asInstanceOf[MemberDef];

    def name : Name = { treex.name; }

    override def replacedBy(tree0 : Tree) : Boolean = if (super.replacedBy(tree0) && tree0.isInstanceOf[MemberDef]) {
      val tree1 = tree0.asInstanceOf[MemberDef];
      treex.name == tree1.name;
    } else false;

    override def update(tree0 : Tree): Boolean = {
      val updated = tree == null || (treex.mods != tree0.asInstanceOf[MemberDef].mods);
      super.update(tree0) || updated;
    }
  }
  abstract class MemberComposite(parent0: Composite) extends MemberMod(parent0) with Composite;

  [_trait_] abstract class HasClassObjects extends Composite {
    override def isMember(tree : Tree) : Boolean = (super.isMember(tree) ||
      (tree.isInstanceOf[ImplDef]));
  }
  abstract class ValOrDefMod(parent0: Composite) extends MemberComposite(parent0) with HasClassObjects {
    def treey = tree.asInstanceOf[ValOrDefDef];
    override def replacedBy(tree0 : Tree) : Boolean = (super.replacedBy(tree0) && tree0.isInstanceOf[ValOrDefDef]);

    override def update(tree0 : Tree): Boolean = {
      val tree1 = tree0.asInstanceOf[ValOrDefDef];
      val updated = tree == null || treex.tpe != tree1.tpe;
      update0(flatten(tree1.rhs, (tree2 : Tree) => isMember(tree2)));
      super.update(tree0) || updated;
    }
  }
  class ValMod(parent0: Composite) extends ValOrDefMod(parent0) {
    def treez = tree.asInstanceOf[ValDef];
    override def replacedBy(tree0 : Tree) : Boolean = (super.replacedBy(tree0) && tree0.isInstanceOf[ValDef]);
  }
  class DefMod(parent0: Composite) extends ValOrDefMod(parent0) {
    def treez = tree.asInstanceOf[DefDef];

    override def replacedBy(tree0 : Tree) : Boolean = if (super.replacedBy(tree0) && tree0.isInstanceOf[DefDef]) {
      val tree1 = tree0.asInstanceOf[DefDef];
      if (tree1.vparamss.length == treez.vparamss.length) {
				val tpz = for (val vd <- treez.vparamss) yield for (val xd <- vd) yield xd.tpe;
				val tp1 = for (val vd <- tree1.vparamss) yield for (val xd <- vd) yield xd.tpe;
				tpz == tp1;
      } else false;
    } else false;
  }
  abstract class ImplMod(parent0: Composite) extends MemberComposite(parent0) with HasClassObjects {
    def treey = tree.asInstanceOf[ImplDef];
    override def replacedBy(tree0 : Tree) : Boolean = (super.replacedBy(tree0) && tree0.isInstanceOf[ImplDef]);
    override def isMember(tree : Tree) : Boolean = (super.isMember(tree) ||
      (tree.isInstanceOf[ValOrDefDef]
       /* && !tree.asInstanceOf[ValOrDefDef].mods.isPrivate */
       /* && !tree.asInstanceOf[ValOrDefDef].mods.isAccessor */) ||
      tree.isInstanceOf[AliasTypeDef]);


    override def member(tree : Tree, members : List[Tree]) : Tree = {
      val tree0 = if (tree.isInstanceOf[DefDef]) {
				val ddef = tree.asInstanceOf[DefDef];
				if (ddef.mods.isAccessor && ddef.symbol != null) {
				  val sym = ddef.symbol.accessed;
				  val ret = for (val member <- members; member.symbol == sym) yield member;
				  if (ret.isEmpty) null;
				  else ret.head;
				} else tree;
      } else super.member(tree, members);
      def sym = tree0.symbol;
      if (tree0 == null || sym.pos == Position.NOPOS) null;
      else tree0;
    }


    def children(tree0 : Tree) : List[Tree] = tree0.asInstanceOf[ImplDef].impl.body;

    override def update(tree0 : Tree): Boolean = {
      var updated = update0(children(tree0));
      (super.update(tree0) || updated);
    }
  }
  class  ClassMod(parent0: Composite) extends ImplMod(parent0) {
    def treez = tree.asInstanceOf[ClassDef];
    override def replacedBy(tree0 : Tree) : Boolean = (super.replacedBy(tree0) && tree0.isInstanceOf[ClassDef]);
  }
  class ObjectMod(parent0: Composite) extends ImplMod(parent0) {
    def treez = tree.asInstanceOf[ModuleDef];
    override def replacedBy(tree0 : Tree) : Boolean = (super.replacedBy(tree0) && tree0.isInstanceOf[ModuleDef]);
  }
  class AliasTypeMod(parent0: Composite) extends MemberMod(parent0) {
    def treey = tree.asInstanceOf[AliasTypeDef];
    override def replacedBy(tree0 : Tree) : Boolean = (super.replacedBy(tree0) && tree0.isInstanceOf[AliasTypeDef]);
  }
  class AbsTypeMod(parent0: Composite) extends HasTree(parent0) {
    def treey = tree.asInstanceOf[AbsTypeDef];
    override def replacedBy(tree0 : Tree) : Boolean = (super.replacedBy(tree0) && tree0.isInstanceOf[AbsTypeDef]);
  }
  class SourceMod(val original : CompilationUnit) extends Composite with HasClassObjects {
    update(original);
    var listener : Listener = null;
    def update(unit : CompilationUnit) = unit.body match {
      case pdef : PackageDef => update0(pdef.stats);
      case _ =>
    }
    override def    add(from : Composite, model : HasTree) : Unit = if (listener != null) { listener.add(from, model); }
    override def remove(from : Composite, model : HasTree) : Unit = if (listener != null) { listener.remove(from, model); }


    override def isMember(tree : Tree) : Boolean = super.isMember(tree) || tree.isInstanceOf[Import];
  }

  def flatten0(exprs : List[Tree], filter: (Tree) => Boolean) : List[Tree] =
    for (val expr <- exprs; val t : Tree <- flatten(expr,filter)) yield t;

  def flatten(expr : Tree, filter: (Tree) => Boolean) : List[Tree] =
    if (filter(expr)) expr :: Nil; else expr match {
      case Block(stats,last) => flatten0(stats, filter) ::: flatten(last, filter);
      case If(cond,thenp,elsep) => flatten(cond,filter) ::: flatten(thenp,filter) ::: flatten(elsep,filter);
      case Assign(lhs,rhs) => flatten(rhs,filter);
      case CaseDef(pat,guard,body) => flatten(body,filter);
      case Return(expr0) => flatten(expr0,filter);
      case  Throw(expr0) => flatten(expr0,filter);
      case Try(block,catches,finalizer) => flatten(block,filter) ::: flatten(finalizer,filter) ::: flatten0(catches, filter);
      case Match(selector,cases) => flatten(selector,filter) ::: flatten0(cases, filter);
      case Apply(fun,args) => flatten(fun,filter) ::: flatten0(args,filter);
      case TypeApply(fun,args) => flatten(fun,filter) ::: flatten0(args,filter);
      case _ => Nil;
    }


  def modelFor(tree: Tree, parent: Composite): HasTree = tree match {
    case _:    ValDef => new    ValMod(parent);
    case _:    DefDef => new    DefMod(parent);
    case _:  ClassDef => new  ClassMod(parent);
    case _: ModuleDef => new ObjectMod(parent);
    case _: AliasTypeDef => new AliasTypeMod(parent);
    case _: AbsTypeDef => new AbsTypeMod(parent);
    case _: Import => new ImportMod(parent);
  }

}
