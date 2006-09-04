/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author Martin Odersky
 */
// $Id$

package scala.tools.nsc.transform

import symtab._
import Flags._
import scala.collection.mutable.ListBuffer
import scala.tools.nsc.util.Position

abstract class Mixin extends InfoTransform {
  import global._
  import definitions._
  import posAssigner.atPos

  /** the following two members override abstract members in Transform */
  val phaseName: String = "mixin"

  override def phaseNewFlags: long = lateMODULE | notABSTRACT

  private def isForwarded(sym: Symbol) = (
    sym.owner.isImplClass && sym.isMethod &&
    (!sym.isModule || sym.hasFlag(PRIVATE)) && !(sym hasFlag (ACCESSOR | SUPERACCESSOR))
  )

  private def isStatic(sym: Symbol) = isForwarded(sym) && sym.isImplOnly

  private def toInterface(tp: Type): Type =
    atPhase(currentRun.mixinPhase)(tp.symbol.toInterface).tpe

  private val toInterfaceMap = new TypeMap {
    def apply(tp: Type): Type = mapOver( tp match {
      case TypeRef(pre, sym, args) if (sym.isImplClass) =>
        typeRef(pre, atPhase(currentRun.mixinPhase)(sym.toInterface), args)
      case _ => tp
    })
  }

  private def rebindSuper(base: Symbol, member: Symbol, prevowner: Symbol): Symbol =
    atPhase(currentRun.refchecksPhase) {
      var bcs = base.info.baseClasses.dropWhile(prevowner !=).tail
      assert(!bcs.isEmpty/*, "" + prevowner + " " + base.info.baseClasses*/)//DEBUG
      var sym: Symbol = NoSymbol
      if (settings.debug.value)
        log("starting rebindsuper " + base + " " + member + ":" + member.tpe +
            " " + prevowner + " " + base.info.baseClasses)
      while (!bcs.isEmpty && sym == NoSymbol) {
        if (settings.debug.value) {
          val other = bcs.head.info.nonPrivateDecl(member.name);
          log("rebindsuper " + bcs.head + " " + other + " " + other.tpe + " " + other.hasFlag(DEFERRED))
        }
        sym = member.overridingSymbol(bcs.head).suchThat(sym => !sym.hasFlag(DEFERRED | BRIDGE))
        bcs = bcs.tail
      }
      assert(sym != NoSymbol, member)
      sym
    }

  private def implClass(iface: Symbol): Symbol = erasure.implClass(iface)

  def addMember(clazz: Symbol, member: Symbol): Symbol = {
    if (settings.debug.value) log("new member of " + clazz + ":" + member.defString)//debug
    clazz.info.decls enter member
    member setFlag MIXEDIN
  }

  def addLateInterfaceMembers(clazz: Symbol): unit =
    if (!(clazz hasFlag MIXEDIN)) {
      clazz setFlag MIXEDIN
      def newGetter(field: Symbol): Symbol =
        clazz.newMethod(field.pos, nme.getterName(field.name))
          .setFlag(field.flags & ~(PRIVATE | LOCAL) | ACCESSOR | DEFERRED)
          .setInfo(MethodType(List(), field.info))
      def newSetter(field: Symbol): Symbol =
        clazz.newMethod(field.pos, nme.getterToSetter(nme.getterName(field.name)))
          .setFlag(field.flags & ~(PRIVATE | LOCAL) | ACCESSOR | DEFERRED)
          .setInfo(MethodType(List(field.info), UnitClass.tpe))
      clazz.info
      val impl = implClass(clazz)
      assert(impl != NoSymbol)
      for (val member <- impl.info.decls.toList) {
        if (!member.isMethod && !member.isModule && !member.isModuleVar) {
          assert(member.isTerm && !member.hasFlag(DEFERRED), member)
          if (member.getter(impl) hasFlag PRIVATE) member.makeNotPrivate(clazz)
          var getter = member.getter(clazz)
          if (getter == NoSymbol) getter = addMember(clazz, newGetter(member))
          else getter setFlag (member getFlag MUTABLE)
          if (!member.tpe.isInstanceOf[ConstantType]) {
            var setter = member.setter(clazz)
            if (setter == NoSymbol) setter = addMember(clazz, newSetter(member))
          }
        }
      }
      if (settings.debug.value) log("new defs of " + clazz + " = " + clazz.info.decls);
    }

  def addMixedinMembers(clazz: Symbol): unit = {
    if (!(clazz hasFlag MIXEDIN) && (clazz != ObjectClass)) {
      assert(!clazz.isTrait, clazz)
      clazz setFlag MIXEDIN
      assert(!clazz.info.parents.isEmpty, clazz)
      val superclazz = clazz.info.parents.head.symbol
      addMixedinMembers(superclazz)
      //System.out.println("adding members of " + clazz.info.baseClasses.tail.takeWhile(superclazz !=) + " to " + clazz);//DEBUG
      val mixins = clazz.info.baseClasses.tail.takeWhile(superclazz !=)
      def mixinMembers(mixinClass: Symbol): unit = {
        //Console.println("mixin members of "+mixinClass+" to "+clazz+" "+clazz.info.baseClasses)//DEBUG
        if (mixinClass.isImplClass) {
          addLateInterfaceMembers(mixinClass.toInterface)
          for (val member <- mixinClass.info.decls.toList) {
            if (isForwarded(member) && !member.isImplOnly) {
              val imember = member.overriddenSymbol(mixinClass.toInterface)
              if (imember.overridingSymbol(clazz) == NoSymbol &&
                  atPhase(phase.next)(clazz.info)
                    .findMember(member.name, 0, lateDEFERRED, false).alternatives.contains(imember)) {
                val member1 = addMember(
                  clazz,
                  member.cloneSymbol(clazz) setPos clazz.pos resetFlag (DEFERRED | lateDEFERRED))
                member1.asInstanceOf[TermSymbol] setAlias member;
              }
            }
          }
        } else if (mixinClass.hasFlag(lateINTERFACE)) {
          addLateInterfaceMembers(mixinClass)
          val impl = implClass(mixinClass)
          //System.out.println("late impl " + mixinClass + " " + impl);//DEBUG
          if (!(mixins contains impl)) mixinMembers(impl)
          for (val member <- mixinClass.info.decls.toList) {
            if (member hasFlag ACCESSOR) {
              val member1 = addMember(
                clazz,
                member.cloneSymbol(clazz)
                  setPos clazz.pos
                  setFlag FINAL resetFlag (DEFERRED | lateDEFERRED));
              if (!member.isSetter)
                member.tpe match {
                  case MethodType(List(), ConstantType(_)) =>
                    ;
                  case _ =>
                    addMember(clazz,
                              clazz.newValue(member.pos, nme.getterToLocal(member.name))
                              setFlag (LOCAL | PRIVATE | member.getFlag(MUTABLE))
                              setInfo member.tpe.resultType)
                }
            } else if (member hasFlag SUPERACCESSOR) {
              val member1 = addMember(clazz, member.cloneSymbol(clazz)) setPos clazz.pos
              assert(member1.alias != NoSymbol, member1)
              val alias1 = rebindSuper(clazz, member.alias, mixinClass)
              member1.asInstanceOf[TermSymbol] setAlias alias1
            } else if (member.isMethod && member.isModule && !(member hasFlag (LIFTED | BRIDGE))) {
              addMember(clazz, member.cloneSymbol(clazz))
                .setPos(clazz.pos)
                .resetFlag(DEFERRED | lateDEFERRED)
            }
          }
        }
      }
//      for (val mixinClass <- mixins) if (mixinClass.hasFlag(lateINTERFACE)) addLateInterfaceMembers(mixinClass);
      for (val mixinClass <- mixins) mixinMembers(mixinClass)
      if (settings.debug.value) log("new defs of " + clazz + " = " + clazz.info.decls)
    }
  }

  override def transformInfo(sym: Symbol, tp: Type): Type = tp match {
    case ClassInfoType(parents, decls, clazz) =>
      assert(clazz.info eq tp, tp)
      assert(sym == clazz, tp)
      var parents1 = parents
      var decls1 = decls
      if (!clazz.isPackageClass) {
        atPhase(phase.next)(clazz.owner.info)
        if (clazz.isImplClass) {
          clazz setFlag lateMODULE
          var sourceModule = clazz.owner.info.decls.lookup(sym.name.toTermName)
          if (sourceModule != NoSymbol) {
            sourceModule setPos sym.pos
            sourceModule.flags = MODULE | FINAL
          } else {
            sourceModule = clazz.owner.newModule(
              sym.pos, sym.name.toTermName, sym.asInstanceOf[ClassSymbol])
            clazz.owner.info.decls enter sourceModule
          }
          sourceModule setInfo sym.tpe
          assert(clazz.sourceModule != NoSymbol)//debug
          parents1 = List()
          decls1 = newScope(decls.toList filter isForwarded)
        } else if (!parents.isEmpty) {
          parents1 = parents.head :: (parents.tail map toInterface)
        }
      }
      //decls1 = atPhase(phase.next)(newScope(decls1.toList))//debug
      if ((parents1 eq parents) && (decls1 eq decls)) tp
      else ClassInfoType(parents1, decls1, clazz)

    case MethodType(formals, restp) =>
      toInterfaceMap(
        if (isForwarded(sym)) MethodType(toInterface(sym.owner.typeOfThis) :: formals, restp)
        else tp)

    case _ =>
      tp
  }

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new MixinTransformer

  class MixinTransformer extends Transformer {
    private var self: Symbol = _
    private val rootContext = erasure.NoContext.make(EmptyTree, RootClass, newScope)
    private var localTyper: erasure.Typer = _
    private var enclInterface: Symbol = _

    private def preTransform(tree: Tree): Tree = {
      val sym = tree.symbol
      tree match {
        case Template(parents, body) =>
          localTyper = erasure.newTyper(rootContext.make(tree, currentOwner))
          atPhase(phase.next)(currentOwner.owner.info)//needed?
          if (!currentOwner.isTrait) addMixedinMembers(currentOwner)
          else if (currentOwner hasFlag lateINTERFACE) addLateInterfaceMembers(currentOwner);
          tree
        case DefDef(mods, name, tparams, List(vparams), tpt, rhs) if currentOwner.isImplClass =>
          if (isForwarded(sym)) {
            sym setFlag notOVERRIDE;
            self = sym.newValue(sym.pos, nme.SELF)
              .setFlag(PARAM)
              .setInfo(toInterface(currentOwner.typeOfThis));
            enclInterface = currentOwner.toInterface;
            val selfdef = ValDef(self) setType NoType;
            copy.DefDef(tree, mods, name, tparams, List(selfdef :: vparams), tpt, rhs)
          } else {
            EmptyTree
          }
        case Apply(tapp @ TypeApply(fn, List(arg)), List()) =>
          if (arg.tpe.symbol.isImplClass) {
            val ifacetpe = toInterface(arg.tpe)
            arg.tpe = ifacetpe
            tapp.tpe = MethodType(List(), ifacetpe)
            tree.tpe = ifacetpe
          }
          tree
        case ValDef(_, _, _, _) if (currentOwner.isImplClass) =>
          EmptyTree
        case _ =>
          tree
      }
    }

    private def selfRef(pos: PositionType) = gen.mkAttributedIdent(self) setPos pos;

    private def staticRef(sym: Symbol) = {
      sym.owner.info
      sym.owner.owner.info
      if (sym.owner.sourceModule == NoSymbol) {
        assert(false, "" + sym + " in " + sym.owner + " in " + sym.owner.owner +
                      " " + sym.owner.owner.info.decls.toList)//debug
      }
      Select(gen.mkAttributedRef(sym.owner.sourceModule), sym)
    }

    private def addNewDefs(clazz: Symbol, stats: List[Tree]): List[Tree] = {
      val newDefs = new ListBuffer[Tree]
      def attributedDef(pos: PositionType, tree: Tree): Tree = {
        if (settings.debug.value) System.out.println("add new def to " + clazz + ": " + tree);
        localTyper.typed { atPos(pos) { tree } }
      }
      def position(sym: Symbol) =
        if (sym.pos == NoPos) clazz.pos else sym.pos
      def addDef(pos: PositionType, tree: Tree): unit =
        newDefs += attributedDef(pos, tree)
      def addDefDef(sym: Symbol, rhs: List[Symbol] => Tree): unit =
        addDef(position(sym), DefDef(sym, vparamss => rhs(vparamss.head)))
      def add(stats: List[Tree], newDefs: List[Tree]) = {
        val newSyms = newDefs map (.symbol)
        def isNotDuplicate(tree: Tree) = tree match {
          case DefDef(_, _, _, _, _, _) =>
            val sym = tree.symbol;
            !((sym hasFlag DEFERRED) &&
              (newSyms exists (nsym => nsym.name == sym.name && (nsym.tpe matches sym.tpe))))
          case _ =>
            true
        }
        if (newDefs.isEmpty) stats
        else newDefs ::: stats.filter(isNotDuplicate)//!!!
      }
      def completeSuperAccessor(stat: Tree) = stat match {
        case DefDef(mods, name, tparams, List(vparams), tpt, EmptyTree)
        if (stat.symbol hasFlag SUPERACCESSOR) =>
          val rhs0 =
            Apply(Select(Super(clazz, nme.EMPTY.toTypeName), stat.symbol.alias),
                  vparams map (vparam => Ident(vparam.symbol)));
          val rhs1 = localTyper.typed(atPos(stat.pos)(rhs0), stat.symbol.tpe.resultType);
          val rhs2 = atPhase(currentRun.mixinPhase)(transform(rhs1))
          if (settings.debug.value)
            log("complete super acc " + stat.symbol + stat.symbol.locationString +
                " " + rhs1 + " " + stat.symbol.alias + stat.symbol.alias.locationString +
                "/" + stat.symbol.alias.owner.hasFlag(lateINTERFACE))//debug
          copy.DefDef(stat, mods, name, tparams, List(vparams), tpt, rhs2)
        case _ =>
          stat
      }
      def implementPrivateObject(stat: Tree): List[Tree] = {
        val sym = stat.symbol
        stat match {
          case _: DefDef if (sym.isModule && sym.owner.isClass && sym.hasFlag(PRIVATE)) =>
            val vdef = attributedDef(position(sym), gen.mkModuleVarDef(sym))
            val adef = attributedDef(
              position(sym),
              DefDef(sym, { vparamss =>
                val args = vparamss.head.map(Ident).take(
                  vdef.symbol.primaryConstructor.info.paramTypes.length)
                gen.mkCached(
                  vdef.symbol,
                  New(TypeTree(vdef.symbol.tpe), List(args)))
               }))
            sym resetFlag lateDEFERRED
            List(vdef, adef)
          case _ =>
            List(stat)
        }
      }
      for (val sym <- clazz.info.decls.toList) {
        if (sym hasFlag MIXEDIN) {
          if (clazz hasFlag lateINTERFACE) {
            addDefDef(sym, vparamss => EmptyTree)
          } else if (!clazz.isTrait) {
            if (sym hasFlag ACCESSOR) {
              addDefDef(sym, vparams => {
                val accessedRef = sym.tpe match {
                  case MethodType(List(), ConstantType(c)) => Literal(c)
                  case _ => Select(This(clazz), sym.accessed)
                }
                if (sym.isSetter) Assign(accessedRef, Ident(vparams.head)) else accessedRef})
            } else if (sym.isModule && !(sym hasFlag LIFTED)) {
              val vdef = gen.mkModuleVarDef(sym)
              addDef(position(sym), vdef)
              addDef(position(sym), gen.mkModuleAccessDef(sym, vdef.symbol))
            } else if (!sym.isMethod) {
              addDef(position(sym), ValDef(sym))
            } else if (sym hasFlag SUPERACCESSOR) {
              addDefDef(sym, vparams => EmptyTree)
            } else {
              assert(sym.alias != NoSymbol, sym)
              addDefDef(sym, vparams =>
                Apply(staticRef(sym.alias), gen.mkAttributedThis(clazz) :: (vparams map Ident)))
            }
          }
        }
      }
      var stats1 = add(stats, newDefs.toList)
      if (!clazz.isTrait) stats1 = stats1 map completeSuperAccessor
      if (clazz.isImplClass) stats1 = stats1 flatMap implementPrivateObject
      stats1
    }

    private def postTransform(tree: Tree): Tree = {
      val sym = tree.symbol
      if (tree.tpe.symbol.isImplClass &&
          (tree.symbol == null || !tree.symbol.isImplClass))
        tree.tpe = toInterface(tree.tpe);
      tree match {
        case Template(parents, body) =>
          val parents1 = currentOwner.info.parents map (t => TypeTree(t) setPos tree.pos)
          val body1 = addNewDefs(currentOwner, body)
          copy.Template(tree, parents1, body1)
        case Apply(TypeApply(sel @ Select(qual, name), List(targ)), List())
        if (tree.symbol == Object_asInstanceOf && (qual.tpe <:< targ.tpe)) =>
          qual
        case Apply(Select(qual, _), args) =>
          def staticCall(target: Symbol) = {
            if (target == NoSymbol)
              assert(false, "" + sym + ":" + sym.tpe + " " + sym.owner + " " + implClass(sym.owner) + " " + implClass(sym.owner).info.member(sym.name) + " " + atPhase(phase.prev)(implClass(sym.owner).info.member(sym.name).tpe) + " " + phase);//debug
            localTyper.typed {
              atPos(tree.pos) {
                val qual1 =
                  if (!qual.isInstanceOf[Super]) qual
                  else if (currentOwner.enclClass.isImplClass) selfRef(qual.pos)
                  else gen.mkAttributedThis(currentOwner.enclClass);
                Apply(staticRef(target), qual1 :: args)
              }
            }
          }
          assert(sym != NoSymbol, tree);//debug
          if (isStatic(sym)) {
            //assert(sym.isConstructor || currentOwner.enclClass.isImplClass, tree);
            staticCall(sym)
          } else qual match {
            case Super(_, mix) =>
              if (mix == nme.EMPTY.toTypeName) {
                if (currentOwner.enclClass.isImplClass)
                  assert(false, "illegal super in trait: " + currentOwner.enclClass + " " + tree);
              }
              if (sym.owner hasFlag lateINTERFACE)
                staticCall(atPhase(phase.prev)(sym.overridingSymbol(implClass(sym.owner))))
              else {
                assert(!(sym.owner hasFlag INTERFACE))
                assert(!currentOwner.enclClass.isImplClass)
                tree
              }
            case _ =>
              tree
          }

        case This(_) if tree.symbol.isImplClass =>
          assert(tree.symbol == currentOwner.enclClass)
          selfRef(tree.pos)
        case Select(Super(_, _), name) =>
          tree
        case Select(qual, name) if sym.owner.isImplClass && !isStatic(sym) =>
          if (sym.isMethod)
            Console.println("####" + sym + sym.isImplOnly + " " + flagsToString(sym.flags))
          assert(!sym.isMethod, sym)
          val getter = sym.getter(enclInterface)
          assert(getter != NoSymbol)
          localTyper.typed {
            atPos(tree.pos) {
              Apply(Select(qual, getter), List())
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
        val tree1 = super.transform(preTransform(tree))
        atPhase(phase.next)(postTransform(tree1))
      } catch {
        case ex: Throwable =>
          System.out.println("exception when traversing " + tree)
        throw ex
      }
    }
  }
}
