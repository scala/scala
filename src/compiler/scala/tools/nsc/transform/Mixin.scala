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

  /** The name of the phase: */
  val phaseName: String = "mixin"

  /** The phase might set the fiollowing new flags: */
  override def phaseNewFlags: long = lateMODULE | notABSTRACT

// --------- helper functions -----------------------------------------------

  /** A member of a trait is implemented statically if its implementation after the
   *  mixin transform is in the static implementation module. To be statically
   *  implemented, a member must be a method that belonged to the trait's implementation class
   *  before (e.g. it is not abstract). Not statically implemented are
   *   - non-private modules: these are implemented directly in the mixin composition class
   *     (private modules, on the other hand, are implemented statically, but their
   *      module variable is not. all such private modules are lifted, because
   *      non-lifted private modules have been eliminated in ExplicitOuter)
   *   - field accessors and superaccessors
   */
  private def isImplementedStatically(sym: Symbol) =
    sym.owner.isImplClass && sym.isMethod &&
    (!sym.isModule || sym.hasFlag(PRIVATE)) &&
    !(sym hasFlag (ACCESSOR | SUPERACCESSOR))

  /** A member of a trait is static only if it belongs only to the
   *  implementation class, not the interface, and it is implemented
   *  statically.
   */
  private def isStaticOnly(sym: Symbol) =
    isImplementedStatically(sym) && sym.isImplOnly

  /** A member of a trait is forwarded if it is implemented statically and it
   *  is also visible in the trait's interface. In that case, a forwarder to
   *  the member's static implementation will be added to the class that
   *  inherits the trait.
   */
  private def isForwarded(sym: Symbol) =
    isImplementedStatically(sym) && !sym.isImplOnly && !sym.isCaseFactory

  /** Maps the type of an implementation class to its interface;
   *  maps all other types to themselves.
   */
  private def toInterface(tp: Type): Type =
    atPhase(currentRun.mixinPhase)(tp.symbol.toInterface).tpe

  /** Maps all parts of this type that refer to implementation classes to
   *  their corresponding interfaces.
   */
  private val toInterfaceMap = new TypeMap {
    def apply(tp: Type): Type = mapOver( tp match {
      case TypeRef(pre, sym, args) if (sym.isImplClass) =>
        typeRef(pre, atPhase(currentRun.mixinPhase)(sym.toInterface), args)
      case _ => tp
    })
  }

  /** The implementation class corresponding to a currently compiled interface.
   *  todo: try to use Symbol.implClass instead?
   */
  private def implClass(iface: Symbol): Symbol = {
    val impl = iface.implClass
    if (impl != NoSymbol) impl else erasure.implClass(iface)
  }

  /** Returns the symbol that is accessed by a super-accessor in a mixin composition.
   *
   *  @param base       The class in mwhich everything is mixed together
   *  @param member     The symbol statically referred to by the superaccessor in the trait
   *  @param mixinClass The mixin class that produced the superaccessor
   */
  private def rebindSuper(base: Symbol, member: Symbol, mixinClass: Symbol): Symbol =
    atPhase(currentRun.refchecksPhase) {
      var bcs = base.info.baseClasses.dropWhile(mixinClass !=).tail
      var sym: Symbol = NoSymbol
      if (settings.debug.value)
        log("starting rebindsuper " + base + " " + member + ":" + member.tpe +
            " " + mixinClass + " " + base.info.baseClasses)
      while (!bcs.isEmpty && sym == NoSymbol) {
        if (settings.debug.value) {
          val other = bcs.head.info.nonPrivateDecl(member.name);
          log("rebindsuper " + bcs.head + " " + other + " " + other.tpe +
              " " + other.hasFlag(DEFERRED))
        }
        sym = member.overridingSymbol(bcs.head).suchThat(sym => !sym.hasFlag(DEFERRED | BRIDGE))
        bcs = bcs.tail
      }
      assert(sym != NoSymbol, member)
      sym
    }

// --------- type transformation -----------------------------------------------

  /** Add given member to given class, and mark member as mixed-in.
   */
  def addMember(clazz: Symbol, member: Symbol): Symbol = {
    if (settings.debug.value) log("new member of " + clazz + ":" + member.defString)
    clazz.info.decls enter member
    member setFlag MIXEDIN
  }

  /** Add getters and setters for all non-module fields of an implementation
   *  class to its interface unless they are already present. This is done
   *  only once per class. The mixedin flag is used to remember whether late
   *  members have been added to an interface.
   *
   *  @param clazz ...
   */
  def addLateInterfaceMembers(clazz: Symbol): unit =
    if (!(clazz hasFlag MIXEDIN)) {
      clazz setFlag MIXEDIN
      assert(phase == currentRun.mixinPhase)

      /** Create a new getter. Getters are never private or local. They are
       *  always accessors and deferred. */
      def newGetter(field: Symbol): Symbol =
        clazz.newMethod(field.pos, nme.getterName(field.name))
          .setFlag(field.flags & ~(PRIVATE | LOCAL) | ACCESSOR | lateDEFERRED)
          .setInfo(MethodType(List(), field.info))

      /** Create a new setter. Setters are never private or local. They are
       *  always accessors and deferred. */
      def newSetter(field: Symbol): Symbol =
        clazz.newMethod(field.pos, nme.getterToSetter(nme.getterName(field.name)))
          .setFlag(field.flags & ~(PRIVATE | LOCAL) | ACCESSOR | lateDEFERRED)
          .setInfo(MethodType(List(field.info), UnitClass.tpe))

      clazz.info // make sure info is up to date, so that implClass is set.
      val impl = implClass(clazz)
      assert(impl != NoSymbol)

      for (val member <- impl.info.decls.toList) {
        if (!member.isMethod && !member.isModule && !member.isModuleVar) {
          assert(member.isTerm && !member.hasFlag(DEFERRED), member)
          if (member.getter(impl) hasFlag PRIVATE) {
            member.makeNotPrivate(clazz) // this will also make getter&setter not private
          }
          val getter = member.getter(clazz)
          if (getter == NoSymbol) addMember(clazz, newGetter(member))
          if (!member.tpe.isInstanceOf[ConstantType]) {
            val setter = member.setter(clazz)
            if (setter == NoSymbol) addMember(clazz, newSetter(member))
          }
        }
      }
      if (settings.debug.value) log("new defs of " + clazz + " = " + clazz.info.decls);
    }

  /** Add all members to be mixed in into a (non-trait-) class
   *  These are:
   *    for every mixin trait T that is not also inherited by the superclass:
   *     add late interface members to T and then:
   *      - if a member M of T is forwarded to the implementation class, add
   *        a forwarder for M unless one exists already.
   *        The alias of the forwarder is the static member it forwards to.
   *      - for every abstract accessor in T, add a field and an implementation for that acessor
   *      - for every super accessor in T, add an implementation of that accessor
   *      - for every module in T, add a module
   */
  def addMixedinMembers(clazz: Symbol): unit = {
    if (!(clazz hasFlag MIXEDIN) && (clazz != ObjectClass)) {
      clazz setFlag MIXEDIN

      assert(!clazz.isTrait, clazz)
      assert(!clazz.info.parents.isEmpty, clazz)

      // first complete the superclass with mixed in members
      addMixedinMembers(clazz.superClass)

      //Console.println("adding members of " + clazz.info.baseClasses.tail.takeWhile(superclazz !=) + " to " + clazz);//DEBUG

      /** Mix in members of implementation class mixinClass into class clazz */
      def mixinImplClassMembers(impl: Symbol, iface: Symbol): unit = {
        assert (impl.isImplClass)
        for (val member <- impl.info.decls.toList) {
          if (isForwarded(member)) {
            val imember = member.overriddenSymbol(iface)
            //Console.println("mixin member "+member+":"+member.tpe+member.locationString+" "+imember+" "+imember.overridingSymbol(clazz)+" to "+clazz+" with scope "+clazz.info.decls)//DEBUG
            if (imember.overridingSymbol(clazz) == NoSymbol &&
                clazz.info.findMember(member.name, 0, lateDEFERRED, false).alternatives.contains(imember)) {
                  val member1 = addMember(
                    clazz,
                    member.cloneSymbol(clazz) setPos clazz.pos resetFlag (DEFERRED | lateDEFERRED))
                  member1.asInstanceOf[TermSymbol] setAlias member;
                }
          }
        }
      }

      /** Mix in members of trait mixinClass into class clazz */
      def mixinTraitMembers(mixinClass: Symbol): unit = {
        // For all members of a trait's interface do:
        for (val member <- mixinClass.info.decls.toList) {
          if ((member hasFlag ACCESSOR) &&
              (!(member hasFlag DEFERRED) || (member hasFlag lateDEFERRED))) {
            // mixin field accessors
            val member1 = addMember(
              clazz,
              member.cloneSymbol(clazz)
                setPos clazz.pos
                setFlag FINAL resetFlag (DEFERRED | lateDEFERRED))
            if (!member.isSetter)
              member.tpe match {
                case MethodType(List(), ConstantType(_)) =>
                  // member is a constant; only getter is needed
                  ;
                case _ =>
                  // otherwise mixin a field as well
                  addMember(clazz,
                            clazz.newValue(member.pos, nme.getterToLocal(member.name))
                            setFlag (LOCAL | PRIVATE | member.getFlag(MUTABLE))
                            setInfo member.tpe.resultType)
              }
          } else if (member hasFlag SUPERACCESSOR) { // mixin super accessors
            val member1 = addMember(clazz, member.cloneSymbol(clazz)) setPos clazz.pos
            assert(member1.alias != NoSymbol, member1)
            val alias1 = rebindSuper(clazz, member.alias, mixinClass)
            member1.asInstanceOf[TermSymbol] setAlias alias1

          } else if (member.isMethod && member.isModule && !(member hasFlag (LIFTED | BRIDGE))) {
            // mixin objects: todo what happens with abstract objects?
            addMember(clazz, member.cloneSymbol(clazz))
              .setPos(clazz.pos)
              .resetFlag(DEFERRED | lateDEFERRED)
          }
        }
      }

      for (val mc <- clazz.mixinClasses)
        if (mc hasFlag lateINTERFACE) {
          addLateInterfaceMembers(mc)
          mixinTraitMembers(mc)
          mixinImplClassMembers(implClass(mc), mc)
        }
    }
  }

  /** The info transform for this phase does the following:
   *   - The parents of every class are mapped from implementation class to interface
   *   - Implementation classes become modules that inherit nothing
   *     and that define all.
   *
   *  @param sym ...
   *  @param tp  ...
   *  @return    ...
   */
  override def transformInfo(sym: Symbol, tp: Type): Type = tp match {
    case ClassInfoType(parents, decls, clazz) =>
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
          decls1 = newScope(decls.toList filter isImplementedStatically)
        } else if (!parents.isEmpty) {
          parents1 = parents.head :: (parents.tail map toInterface)
        }
      }
      //decls1 = atPhase(phase.next)(newScope(decls1.toList))//debug
      if ((parents1 eq parents) && (decls1 eq decls)) tp
      else ClassInfoType(parents1, decls1, clazz)

    case MethodType(formals, restp) =>
      toInterfaceMap(
        if (isImplementedStatically(sym))
          MethodType(toInterface(sym.owner.typeOfThis) :: formals, restp)
        else
          tp)

    case _ =>
      tp
  }

// --------- term transformation -----------------------------------------------

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new MixinTransformer

  class MixinTransformer extends Transformer {

    /** Within a static implementation method: the parameter referring to the
     *  current object undefined evrywhere else.
     */
    private var self: Symbol = _

    /** The rootContext used for typing */
    private val rootContext =
      erasure.NoContext.make(EmptyTree, RootClass, newScope)

    /** The typer */
    private var localTyper: erasure.Typer = _

    /** Within a static implementation method: the interface type corresponding
     *  to the implementation module; undefined evrywhere else.
     */
    private var enclInterface: Symbol = _

    /** The first transform; called in a pre-order traversal at phase mixin
     *  (that is, every node is processed before its children.
     *  What transform does:
     *   - For every non-trait class, add all mixed in members to the class info.
     *   - For every trait, add all late interface members to the class info
     *   - For every static implementation method:
     *       - remove override flag
     *       - create a new method definition that also has a `self' parameter
     *         (which comes first)
     *   - Map implementation class types in type-apply's to their interfaces
     *   - Remove all fields in implementation classes
     */
    private def preTransform(tree: Tree): Tree = {
      val sym = tree.symbol
      tree match {
        case Template(parents, body) =>
          localTyper = erasure.newTyper(rootContext.make(tree, currentOwner))
          atPhase(phase.next)(currentOwner.owner.info)//todo: needed?
          if (!currentOwner.isTrait) addMixedinMembers(currentOwner)
          else if (currentOwner hasFlag lateINTERFACE) addLateInterfaceMembers(currentOwner)
          tree
        case DefDef(mods, name, tparams, List(vparams), tpt, rhs) if currentOwner.isImplClass =>
          if (isImplementedStatically(sym)) {
            sym setFlag notOVERRIDE
            self = sym.newValue(sym.pos, nme.SELF)
              .setFlag(PARAM)
              .setInfo(toInterface(currentOwner.typeOfThis));
            enclInterface = currentOwner.toInterface
            val selfdef = ValDef(self) setType NoType
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
        case ValDef(_, _, _, _) if currentOwner.isImplClass =>
          EmptyTree
        case _ =>
          tree
      }
    }

    /** Create an identifier which references self parameter.
     *
     *  @param pos ...
     */
    private def selfRef(pos: PositionType) =
      gen.mkAttributedIdent(self) setPos pos

    /** Replace a super reference by this or the self parameter, depending
     *  on whether we are in an implementation class or not.
     *  Leave all other trees unchanged */
    private def transformSuper(qual: Tree) =
      if (!qual.isInstanceOf[Super]) qual
      else if (currentOwner.enclClass.isImplClass) selfRef(qual.pos)
      else gen.mkAttributedThis(currentOwner.enclClass)

    /** Create a static reference to given symbol <code>sym</code> of the
     *  form <code>M.sym</code> where M is the symbol's implementation module.
     */
    private def staticRef(sym: Symbol) = {
      sym.owner.info  //todo: needed?
      sym.owner.owner.info //todo: needed?
      if (sym.owner.sourceModule == NoSymbol) {
        assert(false, "" + sym + " in " + sym.owner + " in " + sym.owner.owner +
                      " " + sym.owner.owner.info.decls.toList)//debug
      }
      Select(gen.mkAttributedRef(sym.owner.sourceModule), sym)
    }

    /** Add all new definitions to a non-trait class
     *  These fall into the following categories:
     *    - for a trait interface:
     *       - abstract accessors for all fields in the implementation class
     *    - for a non-trait class:
     *       - A field for every in a mixin class
     *       - Setters and getters for such fields
     *       - module variables and module creators for every module in a mixin class
     *         (except if module is lifted -- in this case the module variable
     *          is local to some function, and the creator method is static.)
     *       - A super accessor for every super accessor in a mixin class
     *       - Forwarders for all methods that are implemented statically
     *  All superaccessors are completed with right-hand sides (@see completeSuperAccessor)
     *  @param clazz  The class to which definitions are added
     */
    private def addNewDefs(clazz: Symbol, stats: List[Tree]): List[Tree] = {
      val newDefs = new ListBuffer[Tree]

      /** Attribute given tree and anchor at given position */
      def attributedDef(pos: PositionType, tree: Tree): Tree = {
        if (settings.debug.value) log("add new def to " + clazz + ": " + tree)
        localTyper.typed { atPos(pos) { tree } }
      }

      /** The position of given symbol, or, if this is undefined,
       *  the position of the current class. */
      def position(sym: Symbol) =
        if (sym.pos == NoPos) clazz.pos else sym.pos

      /** Add tree at given position as new definition */
      def addDef(pos: PositionType, tree: Tree): unit =
        newDefs += attributedDef(pos, tree)

      /** Add new method definition.
       *
       *  @param sym   The method
       *  @param rhs   A function that maps formal parameters to the method's
       *               right-hand side
       */
      def addDefDef(sym: Symbol, rhs: List[Symbol] => Tree): unit =
        addDef(position(sym), DefDef(sym, vparamss => rhs(vparamss.head)))

      /** Add `newdefs' to `stats', removing any abstract method definitions
       *  in <code>stats</code> that are matched by some symbol defined in
       *  <code>newDefs</code>.
       */
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
        else newDefs ::: stats.filter(isNotDuplicate)
      }

      /** If `stat' is a superaccessor, complete it by adding a right-hand side.
       *  (Note: superaccessors are always abstract until this point.
       *   The method to call in a superaccessor is stored in the accessor symbol's alias field.
       *  The rhs is:
       *    super.A(xs)  where A is the super accessor's alias and xs are its formal parameters.
       *  This rhs is typed and then mixin transformed.
       */
      def completeSuperAccessor(stat: Tree) = stat match {
        case DefDef(mods, name, tparams, List(vparams), tpt, EmptyTree)
        if (stat.symbol hasFlag SUPERACCESSOR) =>
          val rhs0 =
            Apply(Select(Super(clazz, nme.EMPTY.toTypeName), stat.symbol.alias),
                  vparams map (vparam => Ident(vparam.symbol)))
          val rhs1 = localTyper.typed(atPos(stat.pos)(rhs0), stat.symbol.tpe.resultType)
          val rhs2 = atPhase(currentRun.mixinPhase)(transform(rhs1))
          if (settings.debug.value)
            log("complete super acc " + stat.symbol + stat.symbol.locationString +
                " " + rhs1 + " " + stat.symbol.alias + stat.symbol.alias.locationString +
                "/" + stat.symbol.alias.owner.hasFlag(lateINTERFACE))//debug
          copy.DefDef(stat, mods, name, tparams, List(vparams), tpt, rhs2)
        case _ =>
          stat
      }

      // begin addNewDefs
      // for all symbols `sym' in the class definition, which are mixed in:
      for (val sym <- clazz.info.decls.toList) {
        if (sym hasFlag MIXEDIN) {
          if (clazz hasFlag lateINTERFACE) {
            // if current class is a trait interface, add an abstract method for accessor `sym'
            addDefDef(sym, vparamss => EmptyTree)
          } else if (!clazz.isTrait) {
            // if class is not a trait:
            if ((sym hasFlag ACCESSOR) &&
                (!(sym hasFlag DEFERRED) || (sym hasFlag lateDEFERRED))) {
              // add accessor definitions
              addDefDef(sym, vparams => {
                val accessedRef = sym.tpe match {
                  case MethodType(List(), ConstantType(c)) => Literal(c)
                  case _ => Select(This(clazz), sym.accessed)
                }
                if (sym.isSetter) Assign(accessedRef, Ident(vparams.head))
                else accessedRef
              })
            } else if (sym.isModule && !(sym hasFlag LIFTED | BRIDGE)) {
              // add modules
              val vdef = gen.mkModuleVarDef(sym)
              addDef(position(sym), vdef)
              addDef(position(sym), gen.mkModuleAccessDef(sym, vdef.symbol))
            } else if (!sym.isMethod) {
              // add fields
              addDef(position(sym), ValDef(sym))
            } else if (sym hasFlag SUPERACCESSOR) {
              // add superaccessors
              addDefDef(sym, vparams => EmptyTree)
            } else {
              // add forwarders
              assert(sym.alias != NoSymbol, sym)
              addDefDef(sym, vparams =>
                Apply(staticRef(sym.alias), gen.mkAttributedThis(clazz) :: (vparams map Ident)))
            }
          }
        }
      }
      var stats1 = add(stats, newDefs.toList)
      if (!clazz.isTrait) stats1 = stats1 map completeSuperAccessor
      stats1
    }

    /** The transform that gets applied to a tree after it has been completely
     *  traversed and possible modified by a preTransform.
     *  This step will
     *    - change every node type that refers to an implementation class to its
     *      corresponding interface, unless the node's symbol is an implementation class.
     *    - change parents of templates to conform to parents in the symbol info
     *    - add all new definitions to a class or interface
     *    - remove widening casts
     *    - change calls to methods which are defined only in implementation classes
     *      to static calls of methods in implementation modules (@see staticCall)
     *    - change super calls to methods in implementation classes to static calls
     *      (@see staticCall)
     *    - change `this' in implementation modules to references to the self parameter
     *    - refer to fields in some implementation class vie an abstract method in the interface.
     */
    private def postTransform(tree: Tree): Tree = {
      val sym = tree.symbol

      // change every node type that refers to an implementation class to its
      // corresponding interface, unless the node's symbol is an implementation class.
      if (tree.tpe.symbol.isImplClass &&
          ((tree.symbol eq null) || !tree.symbol.isImplClass))
        tree.tpe = toInterface(tree.tpe);

      tree match {
        case Template(parents, body) =>
          // change parents of templates to conform to parents in the symbol info
          val parents1 = currentOwner.info.parents map (t => TypeTree(t) setPos tree.pos)

          // add all new definitions to current class or interface
          val body1 = addNewDefs(currentOwner, body)

          copy.Template(tree, parents1, body1)

        case Apply(TypeApply(sel @ Select(qual, name), List(targ)), List())
        if (tree.symbol == Object_asInstanceOf && (qual.tpe <:< targ.tpe)) =>
          // remove widening casts
          qual

        case Apply(Select(qual, _), args) =>
          /** Changes <code>qual.m(args)</code> where m refers to an implementation
           *  class method to Q.m(S, args) where Q is the implementation module of
           *  <code>m</code> and S is the self parameter for the call, which
           *  is determined as follows:
           *     - if qual != super, qual itself
           *     - if qual == super, and we are in an implementation class,
           *       the current self parameter.
           *     - if qual == super, and we are not in an implementation class, `this'
           */
          def staticCall(target: Symbol) = {
            if (target == NoSymbol)
              assert(false, "" + sym + ":" + sym.tpe + " " + sym.owner + " " + implClass(sym.owner) + " " + implClass(sym.owner).info.member(sym.name) + " " + atPhase(phase.prev)(implClass(sym.owner).info.member(sym.name).tpe) + " " + phase);//debug
            localTyper.typed {
              atPos(tree.pos) {
                Apply(staticRef(target), transformSuper(qual) :: args)
              }
            }
          }
          if (isStaticOnly(sym)) {
            // change calls to methods which are defined only in implementation
            // classes to static calls of methods in implementation modules
            staticCall(sym)
          } else qual match {
            case Super(_, mix) =>
              // change super calls to methods in implementation classes to static calls.
              // Transform references super.m(args) as follows:
              //  - if `m' refers to a trait, insert a static call to the correspondign static
              //    implementation
              //  - otherwise return tree unchanged
              if (mix == nme.EMPTY.toTypeName && currentOwner.enclClass.isImplClass)
                assert(false, "illegal super in trait: " + currentOwner.enclClass + " " + tree);
              if (sym.owner hasFlag lateINTERFACE) {
                if (sym.hasFlag(ACCESSOR)) {
                  assert(args.isEmpty)
                  val sym1 = sym.overridingSymbol(currentOwner.enclClass)
                  localTyper.typed {
                    atPos(tree.pos) {
                      Apply(Select(transformSuper(qual), sym1), List())
                    }
                  }
                } else {
                  staticCall(atPhase(phase.prev)(sym.overridingSymbol(implClass(sym.owner))))
                }
              } else {
                assert(!currentOwner.enclClass.isImplClass)
                tree
              }
            case _ =>
              tree
          }

        case This(_) if tree.symbol.isImplClass =>
          // change `this' in implementation modules to references to the self parameter
          assert(tree.symbol == currentOwner.enclClass)
          selfRef(tree.pos)

        case Select(Super(_, _), name) =>
          tree

        case Select(qual, name) if sym.owner.isImplClass && !isStaticOnly(sym) =>
          // refer to fields in some implementation class via an abstract
          // getter in the interface.
          if (sym.isMethod)
            assert(false, "no method allowed here: " + sym + sym.isImplOnly +
                          " " + flagsToString(sym.flags))
          val getter = sym.getter(enclInterface)
          assert(getter != NoSymbol)
          localTyper.typed {
            atPos(tree.pos) {
              Apply(Select(qual, getter), List())
            }
          }

        case Assign(Apply(lhs @ Select(qual, _), List()), rhs) =>
          // assign to fields in some implementation class via an abstract
          // setter in the interface.
          localTyper.typed {
            atPos(tree.pos) {
              Apply(Select(qual, lhs.symbol.setter(enclInterface)) setPos lhs.pos, List(rhs))
            }
          }
        case _ =>
          tree
      }
    }

    /** The main transform method.
     *  This performs pre-order traversal preTransform at mixin phase;
     *  when coming back, it performs a postTransform at phase after.
     */
    override def transform(tree: Tree): Tree = {
      try { //debug
        val tree1 = super.transform(preTransform(tree))
        atPhase(phase.next)(postTransform(tree1))
      } catch {
        case ex: Throwable =>
          Console.println("exception when traversing " + tree)
        throw ex
      }
    }
  }
}
