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
/*
  def toImplClass(sym: Symbol): Symbol = {
    val impl = clazz.owner.info.decl(nme.IMPL_CLASS_NAME(clazz.name));

  }

  def toInterface(sym: Symbol): Symbol;
*/
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

  private def implClass(iface: Symbol): Symbol =
    atPhase(flattenPhase)(iface.owner.info.decl(nme.implClassName(iface.name)));

  override def transformInfo(sym: Symbol, tp: Type): Type = tp match {
    case ClassInfoType(parents, decls, clazz) =>
      var parents1 = parents;
      var decls1 = decls;
      def addMember(member: Symbol): Symbol = {
        if (decls1 eq decls) decls1 = new Scope(decls.toList);
	System.out.println("new member of " + clazz + ":" + member);//debug
        decls1 enter member;
        member
      }
      def newGetter(field: Symbol): Symbol =
        clazz.newMethod(field.pos, nme.getterName(field.name))
          setFlag (field.flags & ~(PRIVATE | LOCAL | MUTABLE) | (ACCESSOR | DEFERRED | SYNTHETIC))
          setInfo MethodType(List(), field.info);
      def newSetter(field: Symbol): Symbol =
        clazz.newMethod(field.pos, nme.getterToSetter(nme.getterName(field.name)))
          setFlag (field.flags & ~(PRIVATE | LOCAL | MUTABLE) | (ACCESSOR | DEFERRED | SYNTHETIC))
          setInfo MethodType(List(field.info), UnitClass.tpe);

      if (clazz.isPackageClass) {
        for (val sym <- decls.elements) {
          if (sym.isImplClass) {
            sym setFlag lateMODULE;
            addMember(
	      clazz.newModule(sym.pos, sym.name.toTermName, sym.asInstanceOf[ClassSymbol])
                setInfo sym.tpe)
          }
        }
      } else if (clazz hasFlag lateINTERFACE) {
        val impl = implClass(clazz);
        assert(impl != NoSymbol, clazz);
        for (val member <- decls.toList) {
          if (!member.isMethod) {
            assert(member.isTerm && !member.hasFlag(DEFERRED));
            member.makeNotPrivate(clazz);
            var getter = member.getter(clazz);
            if (getter == NoSymbol) getter = addMember(newGetter(member));
            getter setFlag FINAL;
            if (!member.tpe.isInstanceOf[ConstantType]) {
              var setter = member.setter(clazz);
              if (getter == NoSymbol) setter = addMember(newSetter(member));
              setter setFlag FINAL
            }
          }
        }
      } else if (clazz.isImplClass) {
	transformInfo(clazz.owner, clazz.owner.info);
        parents1 = List();
        decls1 = new Scope(decls.toList filter isForwarded)
      } else if (!parents.isEmpty) {
        parents1 = parents.head :: (parents.tail map toInterface);
        for (val bc <- clazz.info.baseClasses.tail.takeWhile(parents.head.symbol !=)) {
          if (bc.isImplClass) {
            for (val member <- bc.info.decls.toList) {
              if (isForwarded(member) && (clazz.info.member(member.name).alternatives contains member)) {
                val member1 = member.cloneSymbol(clazz) setFlag MIXEDIN;
		member1.asInstanceOf[TermSymbol] setAlias member;
              } else if (member hasFlag SUPERACCESSOR) {
		member.asInstanceOf[TermSymbol] setAlias rebindSuper(clazz, member.alias, bc);
                addMember(member)
              }
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
