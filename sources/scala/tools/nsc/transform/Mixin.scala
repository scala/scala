/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author
 */
// $Id$
package scala.tools.nsc.transform;

import symtab._;
import Flags._;
import util.{ListBuffer}
import scala.tools.util.Position;

abstract class Mixin extends InfoTransform {
  import global._;
  import definitions._;
  import posAssigner.atPos;

  /** the following two members override abstract members in Transform */
  val phaseName: String = "mixin";

  override def phaseNewFlags: long = lateMODULE;

  def isForwarded(sym: Symbol) =
    sym.owner.isImplClass && sym.isMethod && !(sym hasFlag (ACCESSOR | SUPERACCESSOR));

  def isStatic(sym: Symbol) = isForwarded(sym) && (sym.hasFlag(PRIVATE) || sym.isConstructor);

  def isSelfRef(tree: Tree) = tree match {
    case This(_) => true
    case Super(_, mix) => mix != nme.EMPTY.toTypeName
    case _ => false
  }

  def toInterface(tp: Type): Type =
    if (tp.symbol.isImplClass) {
      val iface = tp.parents.last;
      if (!tp.symbol.name.startsWith(iface.symbol.name)) assert(false, "bad interface: " + tp + " " + iface);
      iface
    } else tp;

  def makeUnique(decls: Scope, sym: Symbol): unit = {
    if ((sym hasFlag PRIVATE) && decls.lookup(sym.name) != NoSymbol) {
      val prefix = sym.name.toString() + "$";
      var index = 0;
      while (decls.lookup(newTermName(prefix + index)) != NoSymbol) index = index + 1;
      sym.name = newTermName(prefix + index)
    }
  }

  private def staticRef(sym: Symbol) = {
    System.out.println("static ref " + sym.owner + " . " + sym);//debug
    sym.owner.info;
    Select(gen.mkRef(sym.owner.sourceModule), sym);
  }

  private def rebindSuper(base: Symbol, member: Symbol, prevowner: Symbol): Symbol = {
    var bcs = base.info.baseClasses.dropWhile(prevowner !=).tail;
    var sym: Symbol = NoSymbol;
    while (!bcs.isEmpty && sym == NoSymbol) {
      System.out.println("rebindsuper " + bcs.head + " " + bcs.head.info.nonPrivateDecl(member.name));//debug
      sym = member.overridingSymbol(bcs.head).suchThat(sym => !sym.hasFlag(DEFERRED));
      bcs = bcs.tail
    }
    assert(sym != NoSymbol, member);
    sym
  }
  override def transformInfo(sym: Symbol, tp: Type): Type = tp match {
    case ClassInfoType(parents, decls, clazz) =>
      var parents1 = parents;
      var decls1 = decls;
      if (clazz.isPackageClass) {
        for (val sym <- decls.elements) {
          if (sym.isImplClass) {
            sym setFlag lateMODULE;
            val sourceModule =
	      clazz.newModule(sym.pos, sym.name.toTermName, sym.asInstanceOf[ClassSymbol])
                setInfo sym.tpe;
            if (decls1 eq decls) decls1 = new Scope(decls.toList);
	    System.out.println("new member of " + clazz + ":" + sourceModule);//debug
            decls1 enter sourceModule;
          }
        }
/*
      } else if (clazz hasFlag lateINTERFACE) {
        val impl = clazz.owner.decl(nme.IMPL_CLASS_NAME(clazz.name));
        assert(impl != NoSymbol, clazz);
	for (val member <- impl.info.decls.toList) {
	  if (!member.isMethod && member.hasFlag(PRIVATE)) {
            addMember(
              clazz.newMethod(member.pos, nme.GETTER_NAME(name))
                setFlag(DEFERRED | ACCESSOR) setInfo member.tpe);
            addMember(
              clazz.newmethod(member.pos, nme.SETTER_NAME(name))
                setFlag(DEFERRED | ACCESSOR) setInfo MethodType(member.tpe, UnitClass.tpe));
                               }


	    if (member.getter == NoSymbol) {
	      if (decls1 eq decls) decls1 = new Scope(decls.toList);
	      decls1 enter member.newGetter.notPrivate;
	    }
	    if (member.setter == NoSymbol) {
	      if (decls1 eq decls) decls1 = new Scope(decls.toList);
	      decls1 enter member.newSetter.notPrivate;
	    }
	}
*/
      } else if (clazz.isImplClass) {
	transformInfo(clazz.owner, clazz.owner.info);
        parents1 = List();
        decls1 = new Scope(decls.toList filter isForwarded)
      } else if (!parents.isEmpty) {
        parents1 = parents.head :: (parents.tail map toInterface);
        for (val bc <- clazz.info.baseClasses.tail.takeWhile(parents.head.symbol !=))
          if (bc.isImplClass) {
            for (val member <- bc.info.decls.toList) {
              if (!isStatic(member) &&
                  ((member hasFlag PRIVATE) ||
                   (clazz.info.member(member.name).alternatives contains member))) {
                if (decls1 eq decls) decls1 = new Scope(decls.toList);
                val member1 = member.cloneSymbol(clazz) setFlag MIXEDIN;
                if (isForwarded(member))
		  member1.asInstanceOf[TermSymbol] setAlias member;
                else if (member1 hasFlag SUPERACCESSOR)
		  member1.asInstanceOf[TermSymbol] setAlias rebindSuper(clazz, member.alias, bc);
		else
		  makeUnique(decls1, member1);
		System.out.println("new member of " + clazz + ":" + member1);//debug
                decls1 enter member1
              }
            }
          }
      }
      if ((parents1 eq parents) && (decls1 eq decls)) tp else ClassInfoType(parents, decls1, clazz)
    case MethodType(formals, restp) =>
      if (isForwarded(sym))
        MethodType(toInterface(sym.owner.thisType) :: formals, restp)
      else tp
    case _ =>
      tp
  }

  protected def newTransformer(unit: CompilationUnit): Transformer = new MixinTransformer;

  class MixinTransformer extends Transformer {

    var self: Symbol = _;
    var localTyper: analyzer.Typer = _;

    private def addNewDefs(clazz: Symbol, stats: List[Tree]): List[Tree] = {
      val newDefs = new ListBuffer[Tree];
      def addDef(pos: int, tree: Tree): unit = {
        System.out.println("add new def to " + clazz + ": " + tree);
        newDefs += localTyper.typed(atPos(pos)(tree))
      }
      def position(sym: Symbol) =
        if (sym.pos == Position.NOPOS) clazz.pos else sym.pos;
      def addDefDef(sym: Symbol, rhs: List[Symbol] => Tree): unit =
	addDef(position(sym), DefDef(sym, vparamss => rhs(vparamss.head)));
      if (!clazz.isImplClass) {
	atPhase(phase.next) {
	  for (val sym <- clazz.info.decls.toList) {
	    if (sym hasFlag MIXEDIN) {
	      if (sym hasFlag SUPERACCESSOR) {
		addDefDef(sym, vparams =>
		  Apply(Select(Super(clazz, nme.EMPTY.toTypeName), sym.alias), vparams map Ident))
	      } else if (sym hasFlag ACCESSOR) {
		addDefDef(sym, vparams =>
		  Select(This(clazz), sym.accessed))
	      } else if (isForwarded(sym)) {
		addDefDef(sym, vparams =>
		  Apply(staticRef(sym.alias), gen.This(clazz) :: (vparams map Ident)))
	      } else {
		addDef(position(sym), ValDef(sym))
	      }
	    }
	  }
	}
      }
      if (newDefs.hasNext) stats ::: newDefs.toList else stats
    }

    override def transform(tree: Tree): Tree = {
     try { //debug
      val sym = tree.symbol;
      val tree1 = tree match {
        case Template(parents, body) =>
	  localTyper = typer.atOwner(tree, currentOwner);
	  tree
        case DefDef(mods, name, tparams, List(vparams), tpt, rhs) =>
          if (isForwarded(sym)) {
            self = sym.newValue(sym.pos, nme.SELF)
	      setFlag PARAM
	      setInfo toInterface(currentOwner.thisType);
            val selfdef = ValDef(self) setType NoType;
            copy.DefDef(tree, mods, name, tparams, List(selfdef :: vparams), tpt, rhs)
          } else if (currentOwner.isImplClass) {
            EmptyTree
          } else tree
        case ValDef(_, _, _, _) if (currentOwner.isImplClass) =>
	  EmptyTree
        case Super(_, mix) if (mix == nme.EMPTY.toTypeName && currentOwner.isImplClass) =>
	  atPos(tree.pos){
	    localTyper.typed {
	      Select(Ident(self), sym.superAccessor(self.tpe.symbol))
	    }
	  }
        case Apply(Select(qual, _), args)
	if (isSelfRef(qual) && currentOwner.enclClass.isImplClass &&
	    sym.owner.isImplClass && isStatic(sym)) =>
	  atPos(tree.pos) {
	    atPhase(phase.next) {
	      localTyper.typed {
		Apply(staticRef(sym), Ident(self) :: args)
	      }
	    }
	  }
	case This(_) if (sym.isImplClass) =>
	  atPos(tree.pos)(gen.Ident(self))
	case _ =>
	  tree
      }
      val tree2 = super.transform(tree1);
      tree2 match {
        case Template(parents, body) =>
          copy.Template(tree2, parents, addNewDefs(currentOwner, body))
        case _ =>
	  tree2
      }
     } catch {//debug
       case ex: Throwable =>
	 System.out.println("exception when traversing " + tree);
       throw ex
     }
    }
  }
}
