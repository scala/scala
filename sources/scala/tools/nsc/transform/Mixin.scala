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

  override def phaseNewFlags: long = lateMODULE | notABSTRACT;

  private def isForwarded(sym: Symbol) =
    sym.owner.isImplClass && sym.isMethod && !(sym hasFlag (ACCESSOR | SUPERACCESSOR));

  private def isStatic(sym: Symbol) = isForwarded(sym) && (sym.hasFlag(PRIVATE) || sym.isConstructor);

  private def toInterface(tp: Type): Type = tp.symbol.toInterface.tpe;

  private def rebindSuper(base: Symbol, member: Symbol, prevowner: Symbol): Symbol = {
    var bcs = base.info.baseClasses.dropWhile(prevowner !=).tail;
    var sym: Symbol = NoSymbol;
    while (!bcs.isEmpty && sym == NoSymbol) {
      if (settings.debug.value) log("rebindsuper " + bcs.head + " " + bcs.head.info.nonPrivateDecl(member.name));
      sym = member.overridingSymbol(bcs.head).suchThat(sym => !sym.hasFlag(DEFERRED));
      bcs = bcs.tail
    }
    assert(sym != NoSymbol, member);
    sym
  }

  private def implClass(iface: Symbol): Symbol = erasure.implClass(iface);

  override def transformInfo(sym: Symbol, tp: Type): Type = tp match {
    case ClassInfoType(parents, decls, clazz) =>
      assert(sym == clazz, "not equal: " + sym + " " + clazz);
      var parents1 = parents;
      var decls1 = decls;
      def addMember(member: Symbol): Symbol = {
        if (decls1 eq decls) decls1 = new Scope(decls.toList);
	if (settings.debug.value) log("new member of " + clazz + ":" + member.defString);//debug
        decls1 enter member;
        member
      }
      def addPackageClassMembers = {
        for (val sym <- decls.toList) {
          if (sym.isImplClass) {
            sym setFlag lateMODULE | notABSTRACT;
            addMember(
	      clazz.newModule(sym.pos, sym.name.toTermName, sym.asInstanceOf[ClassSymbol])
                setInfo sym.tpe)
          }
        }
      }
      def addLateInterfaceMembers = {
        def newGetter(field: Symbol): Symbol =
          clazz.newMethod(field.pos, nme.getterName(field.name))
            setFlag (field.flags & ~(PRIVATE | LOCAL) | ACCESSOR | DEFERRED | SYNTHETIC)
            setInfo MethodType(List(), field.info);
        def newSetter(field: Symbol): Symbol =
          clazz.newMethod(field.pos, nme.getterToSetter(nme.getterName(field.name)))
            setFlag (field.flags & ~(PRIVATE | LOCAL) | ACCESSOR | DEFERRED | SYNTHETIC)
            setInfo MethodType(List(field.info), UnitClass.tpe);
	clazz.info;
        val impl = implClass(clazz);
        assert(impl != NoSymbol, "" + clazz + " " + flattenPhase.flatClasses + atPhase(flattenPhase)(clazz.owner.info.decls));
        for (val member <- impl.info.decls.toList) {
          if (!member.isMethod && !member.isModule && !member.isModuleVar) {
            assert(member.isTerm && !member.hasFlag(DEFERRED), member);
	    if (member.getter(impl) hasFlag PRIVATE) member.makeNotPrivate(clazz);
            var getter = member.getter(clazz);
            if (getter == NoSymbol) getter = addMember(newGetter(member));
            else getter setFlag (member getFlag MUTABLE);
            if (!member.tpe.isInstanceOf[ConstantType]) {
              var setter = member.setter(clazz);
              if (setter == NoSymbol) setter = addMember(newSetter(member));
            }
	  } else if ((member hasFlag LIFTED) && !(member hasFlag PRIVATE)) {
	    member.expandName(clazz);
	    addMember(member.cloneSymbol(clazz));
	  }
	}
      }
      def addMixedinMembers = {
        for (val bc <- clazz.info.baseClasses.tail.takeWhile(parents.head.symbol !=)) {
          if (bc.isImplClass) {
            for (val member <- bc.info.decls.toList) {
              if (isForwarded(member) && !isStatic(member) &&
                  (clazz.info.member(member.name).alternatives contains member)) {
                val member1 = addMember(member.cloneSymbol(clazz) setFlag MIXEDIN);
		member1.asInstanceOf[TermSymbol] setAlias member;
              }
            }
          } else if (bc.hasFlag(lateINTERFACE)) {
            for (val member <- atPhase(phase.next)(bc.info.decls.toList)) {
              if (member hasFlag ACCESSOR) {
                val member1 = addMember(
                  member.cloneSymbol(clazz) setFlag (MIXEDIN | FINAL) resetFlag DEFERRED);
                if (!member.isSetter)
		  member.tpe match {
		    case MethodType(List(), ConstantType(_)) =>
		      ;
		    case _ =>
                      addMember(
			clazz.newValue(member.pos, nme.getterToLocal(member.name))
			setFlag (LOCAL | PRIVATE | MIXEDIN | member.getFlag(MUTABLE))
			setInfo member.tpe.resultType)
                  }
              } else if (member hasFlag SUPERACCESSOR) {
                val member1 = addMember(member.cloneSymbol(clazz)) setFlag MIXEDIN;
                assert(member1.alias != NoSymbol, member1);
                member1.asInstanceOf[TermSymbol] setAlias rebindSuper(clazz, member.alias, bc);
              } else if (member.isMethod && member.isModule && !(member hasFlag LIFTED)) {
                addMember(member.cloneSymbol(clazz) setFlag MIXEDIN)
              }
            }
          }
        }
      }

      if (clazz.isPackageClass) {
        addPackageClassMembers
      } else {
	atPhase(phase.next)(clazz.owner.info);
        if (clazz.isImplClass) {
          parents1 = List();
          decls1 = new Scope(decls.toList filter isForwarded)
        } else if (!parents.isEmpty) {
          parents1 = parents.head :: (parents.tail map toInterface);
          if (!(clazz hasFlag INTERFACE)) addMixedinMembers
          else if (clazz hasFlag lateINTERFACE) addLateInterfaceMembers
        }
      }
      if (settings.debug.value && !clazz.isPackageClass) log("new defs of " + clazz + " = " + decls1);
      decls1 = atPhase(phase.next)(new Scope(decls1.toList));//debug
      if ((parents1 eq parents) && (decls1 eq decls)) tp
      else ClassInfoType(parents1, decls1, clazz);

    case MethodType(formals, restp) =>
      if (isForwarded(sym)) MethodType(toInterface(sym.owner.typeOfThis) :: formals, restp)
      else tp
    case _ =>
      tp
  }

  protected def newTransformer(unit: CompilationUnit): Transformer = new MixinTransformer;

  class MixinTransformer extends Transformer {
    private var self: Symbol = _;
    private var localTyper: analyzer.Typer = _;
    private var enclInterface: Symbol = _;

    private def preTransform(tree: Tree): Tree = {
      val sym = tree.symbol;
      tree match {
        case Template(parents, body) =>
	  localTyper = typer.atOwner(tree, currentOwner);
	  tree
        case DefDef(mods, name, tparams, List(vparams), tpt, rhs) if currentOwner.isImplClass =>
          if (isForwarded(sym)) {
	    sym setFlag notOVERRIDE;
            self = sym.newValue(sym.pos, nme.SELF)
	      setFlag (PARAM | SYNTHETIC)
	      setInfo toInterface(currentOwner.typeOfThis);
	    enclInterface = currentOwner.toInterface;
            val selfdef = ValDef(self) setType NoType;
            copy.DefDef(tree, mods, name, tparams, List(selfdef :: vparams), tpt, rhs)
          } else {
            EmptyTree
          }
        case ValDef(_, _, _, _) if (currentOwner.isImplClass) =>
	  EmptyTree
        case _ =>
          tree
      }
    }

    private def selfRef(pos: int) = gen.Ident(self) setPos pos;

    private def staticRef(sym: Symbol) = {
      sym.owner.info;
      if (sym.owner.sourceModule == NoSymbol)
	assert(false, "" + sym.owner + " " + sym.owner.owner.info.decls);//debug
      Select(gen.mkRef(sym.owner.sourceModule), sym);
    }

    private def addNewDefs(clazz: Symbol, stats: List[Tree]): List[Tree] = {
      val newDefs = new ListBuffer[Tree];
      def addDef(pos: int, tree: Tree): unit = {
        if (settings.debug.value) log("add new def to " + clazz + ": " + tree);
        newDefs += localTyper.typed {
	  atPos(pos) {
	    tree
	  }
	}
      }
      def position(sym: Symbol) =
        if (sym.pos == Position.NOPOS) clazz.pos else sym.pos;
      def addDefDef(sym: Symbol, rhs: List[Symbol] => Tree): unit =
	addDef(position(sym), DefDef(sym, vparamss => rhs(vparamss.head)));
      def completeSuperAccessor(stat: Tree) = stat match {
        case DefDef(mods, name, tparams, List(vparams), tpt, EmptyTree)
        if (stat.symbol hasFlag SUPERACCESSOR) =>
          assert(stat.symbol hasFlag MIXEDIN, stat);
          val rhs1 =
            localTyper.typed {
              atPos(stat.pos) {
                Apply(Select(Super(clazz, nme.EMPTY.toTypeName), stat.symbol.alias),
                      vparams map (vparam => Ident(vparam.symbol)))
              }
            }
          copy.DefDef(stat, mods, name, tparams, List(vparams), tpt, rhs1)
        case _ =>
          assert(!(stat.symbol hasFlag SUPERACCESSOR), stat);
          stat
      }
      var stats1 = stats;
      if (clazz hasFlag lateINTERFACE) {
	for (val sym <- clazz.info.decls.toList) {
	  if ((sym hasFlag SYNTHETIC) && (sym hasFlag ACCESSOR))
	    addDefDef(sym, vparamss => EmptyTree)
	}
      } else if (!clazz.isImplClass && !(clazz hasFlag INTERFACE)) {
	for (val sym <- clazz.info.decls.toList) {
	  if (sym hasFlag MIXEDIN) {
	    if (sym hasFlag ACCESSOR) {
              addDefDef(sym, vparams => {
		val accessedRef = sym.tpe match {
		  case MethodType(List(), ConstantType(c)) => Literal(c)
		  case _ => Select(This(clazz), sym.accessed)
		}
                if (sym.isSetter) Assign(accessedRef, Ident(vparams.head)) else accessedRef})
	    } else if (sym.isModule && !(sym hasFlag LIFTED)) {
              val vdef = refchecks.newModuleVarDef(sym);
              addDef(position(sym), vdef);
              addDef(position(sym), refchecks.newModuleAccessDef(sym, vdef.symbol));
	    } else if (!sym.isMethod) {
	      addDef(position(sym), ValDef(sym))
	    } else if (!(sym hasFlag SUPERACCESSOR)) {
	      assert(sym.alias != NoSymbol, sym);
	      addDefDef(sym, vparams =>
		Apply(staticRef(sym.alias), gen.This(clazz) :: (vparams map Ident)))
	    }
	  }
	}
        stats1 = stats map completeSuperAccessor;
      }
      if (newDefs.hasNext) stats1 ::: newDefs.toList else stats1
    }

    private def postTransform(tree: Tree): Tree = atPhase(phase.next) {
      val sym = tree.symbol;
      tree match {
        case Template(parents, body) =>
          val parents1 = currentOwner.info.parents map (t => TypeTree(t) setPos tree.pos);
	  val body1 = addNewDefs(currentOwner, body);
          copy.Template(tree, parents1, body1)
        case Apply(Select(qual, _), args) =>
          assert(sym != NoSymbol, tree);//debug
          if (isStatic(sym)) {
            assert(sym.isConstructor || currentOwner.enclClass.isImplClass, tree);
	    localTyper.typed {
	      atPos(tree.pos) {
	        Apply(staticRef(sym), qual :: args)
	      }
	    }
          } else tree
        case This(_) if tree.symbol.isImplClass =>
	  assert(tree.symbol == currentOwner.enclClass, "" + tree.symbol + " " + currentOwner.enclClass);
          selfRef(tree.pos)
        case Select(qual @ Super(_, mix), name) =>
          if (currentOwner.enclClass.isImplClass) {
            if (mix == nme.EMPTY.toTypeName) {
	      val superAccName = enclInterface.expandedName(nme.superName(sym.name));
	      val superAcc = enclInterface.info.decl(superAccName) suchThat (.alias.==(sym));
	      assert(superAcc != NoSymbol, tree);//debug
	      localTyper.typedOperator {
	        atPos(tree.pos){
	          Select(selfRef(qual.pos), superAcc)
	        }
	      }
            } else {
              copy.Select(tree, selfRef(qual.pos), name)
            }
          } else {
            if (mix == nme.EMPTY.toTypeName) tree
            else copy.Select(tree, gen.This(currentOwner.enclClass) setPos qual.pos, name)
          }
        case Select(qual, name) if sym.owner.isImplClass && !isStatic(sym) =>
	  if (sym.isMethod) {
	    assert(sym hasFlag LIFTED, sym);
	    val sym1 = enclInterface.info.decl(sym.name);
	    assert(sym1 != NoSymbol && !(sym1 hasFlag OVERLOADED), sym);//debug
	    tree setSymbol sym1
	  } else {
	    val getter = sym.getter(enclInterface);
	    assert(getter != NoSymbol, "" + enclInterface + " " + sym);
            localTyper.typed {
	      atPos(tree.pos) {
		Apply(Select(qual, getter), List())
	      }
            }
	  }
        case Assign(Apply(lhs @ Select(qual, _), List()), rhs) =>
          localTyper.typed {
            atPos(tree.pos) {
              Apply(Select(qual, lhs.symbol.setter(enclInterface)) setPos lhs.pos, List(rhs))
            }
          }
        case _ =>
          tree
      }
    }

    override def transform(tree: Tree): Tree = {
      try { //debug
        postTransform(super.transform(preTransform(tree)))
      } catch {
        case ex: Throwable =>
	  System.out.println("exception when traversing " + tree);
        throw ex
      }
    }
  }
}
