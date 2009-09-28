/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author Martin Odersky
 */
// $Id$

package scala.tools.nsc
package transform

import symtab._
import Flags._
import scala.tools.nsc.util.{Position,NoPosition}
import collection.mutable.{ListBuffer, HashMap}

abstract class Mixin extends InfoTransform with ast.TreeDSL {
  import global._
  import definitions._
  import CODE._

  /** The name of the phase: */
  val phaseName: String = "mixin"

  /** The phase might set the fiollowing new flags: */
  override def phaseNewFlags: Long = lateMODULE | notABSTRACT

  /** This map contains a binding (class -> info) if
   *  the class with this info at phase mixinPhase has been treated for mixin composition
   */
  private val treatedClassInfos = collection.mutable.Map[Symbol, Type]()

// --------- helper functions -----------------------------------------------

  /** A member of a trait is implemented statically if its implementation after the
   *  mixin transform is in the static implementation module. To be statically
   *  implemented, a member must be a method that belonged to the trait's implementation class
   *  before (e.g. it is not abstract). Not statically implemented are
   *   - non-private modules: these are implemented directly in the mixin composition class
   *     (private modules, on the other hand, are implemented statically, but their
   *      module variable is not. all such private modules are lifted, because
   *      non-lifted private modules have been eliminated in ExplicitOuter)
   *   - field accessors and superaccessors, except for lazy value accessors which become initializer
   *     methods in the impl class (because they can have arbitrary initializers)
   */
  private def isImplementedStatically(sym: Symbol) =
    sym.owner.isImplClass && sym.isMethod &&
    (!sym.isModule || sym.hasFlag(PRIVATE | LIFTED)) &&
    (!(sym hasFlag (ACCESSOR | SUPERACCESSOR)) || sym.hasFlag(LAZY))

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
    isImplementedStatically(sym) && !sym.isImplOnly

  /** Maps the type of an implementation class to its interface;
   *  maps all other types to themselves.
   */
  private def toInterface(tp: Type): Type =
    atPhase(currentRun.mixinPhase)(tp.typeSymbol.toInterface).tpe

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
   *  @param base       The class in which everything is mixed together
   *  @param member     The symbol statically referred to by the superaccessor in the trait
   *  @param mixinClass The mixin class that produced the superaccessor
   */
  private def rebindSuper(base: Symbol, member: Symbol, mixinClass: Symbol): Symbol =
    atPhase(currentRun.picklerPhase.next) {
      var bcs = base.info.baseClasses.dropWhile(mixinClass !=).tail
      var sym: Symbol = NoSymbol
      if (settings.debug.value)
        log("starting rebindsuper " + base + " " + member + ":" + member.tpe +
            " " + mixinClass + " " + base.info.baseClasses)
      while (!bcs.isEmpty && sym == NoSymbol) {
        if (settings.debug.value) {
          val other = bcs.head.info.nonPrivateDecl(member.name);
          log("rebindsuper " + bcs.head + " " + other + " " + other.tpe +
              " " + other.isDeferred)
        }
        sym = member.overridingSymbol(bcs.head).suchThat(sym => !sym.hasFlag(DEFERRED | BRIDGE))
        bcs = bcs.tail
      }
      assert(sym != NoSymbol, member)
      sym
    }

// --------- type transformation -----------------------------------------------

  def isConcreteAccessor(member: Symbol) =
    (member hasFlag ACCESSOR) &&
    (!(member hasFlag DEFERRED) || (member hasFlag lateDEFERRED))

  /** Is member overridden (either directly or via a bridge) in base class sequence `bcs'? */
  def isOverriddenAccessor(member: Symbol, bcs: List[Symbol]): Boolean = atPhase(ownPhase) {
    def hasOverridingAccessor(clazz: Symbol) = {
      clazz.info.nonPrivateDecl(member.name).alternatives.exists(
        sym =>
          isConcreteAccessor(sym) &&
          !sym.hasFlag(MIXEDIN) &&
          matchesType(sym.tpe, member.tpe, true))
    }
    bcs.head != member.owner &&
    (hasOverridingAccessor(bcs.head) || isOverriddenAccessor(member, bcs.tail))
  }

  /** Add given member to given class, and mark member as mixed-in.
   */
  def addMember(clazz: Symbol, member: Symbol): Symbol = {
    if (settings.debug.value) log("new member of " + clazz + ":" + member.defString)
    clazz.info.decls enter member
    member setFlag MIXEDIN
  }

  def needsExpandedSetterName(field: Symbol) =
    !(field hasFlag LAZY) &&
    (if (field.isMethod) (field hasFlag STABLE) else !(field hasFlag MUTABLE))

  /** Add getters and setters for all non-module fields of an implementation
   *  class to its interface unless they are already present. This is done
   *  only once per class. The mixedin flag is used to remember whether late
   *  members have been added to an interface.
   *    - lazy fields don't get a setter.
   *
   *  @param clazz ...
   */
  def addLateInterfaceMembers(clazz: Symbol) {
    if ((treatedClassInfos get clazz) != Some(clazz.info)) {
      treatedClassInfos(clazz) = clazz.info
      assert(phase == currentRun.mixinPhase)

      /** Create a new getter. Getters are never private or local. They are
       *  always accessors and deferred. */
      def newGetter(field: Symbol): Symbol = {
        //println("creating new getter for "+field+field.locationString+(field hasFlag MUTABLE))
        clazz.newMethod(field.pos, nme.getterName(field.name))
          .setFlag(field.flags & ~(PRIVATE | LOCAL) | ACCESSOR | lateDEFERRED |
                     (if (field hasFlag MUTABLE) 0 else STABLE))
          .setInfo(MethodType(List(), field.info))
      }

      /** Create a new setter. Setters are never private or local. They are
       *  always accessors and deferred. */
      def newSetter(field: Symbol): Symbol = {
        //println("creating new setter for "+field+field.locationString+(field hasFlag MUTABLE))
        val setterName = nme.getterToSetter(nme.getterName(field.name))
        val setter = clazz.newMethod(field.pos, setterName)
          .setFlag(field.flags & ~(PRIVATE | LOCAL) | ACCESSOR | lateDEFERRED)
        setter.setInfo(MethodType(setter.newSyntheticValueParams(List(field.info)), UnitClass.tpe))
        if (needsExpandedSetterName(field)) {
          //println("creating expanded setter from "+field)
          setter.name = clazz.expandedSetterName(setter.name)
        }
        setter
      }

      clazz.info // make sure info is up to date, so that implClass is set.
      val impl = implClass(clazz)
      assert(impl != NoSymbol)

      for (member <- impl.info.decls.toList) {
        if (!member.isMethod && !member.isModule && !member.isModuleVar) {
          assert(member.isTerm && !member.isDeferred, member)
          if (member.getter(impl) hasFlag PRIVATE) {
            member.makeNotPrivate(clazz) // this will also make getter&setter not private
          }
          val getter = member.getter(clazz)
          if (getter == NoSymbol) addMember(clazz, newGetter(member))
          if (!member.tpe.isInstanceOf[ConstantType] && !member.hasFlag(LAZY)) {
            val setter = member.setter(clazz, needsExpandedSetterName(member))
            if (setter == NoSymbol) addMember(clazz, newSetter(member))
          }
        }
      }
      if (settings.debug.value) log("new defs of " + clazz + " = " + clazz.info.decls);
    }
  }

  /** Map a lazy, mixedin field accessor to it's trait member accessor */
  val initializer = new HashMap[Symbol, Symbol]

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
  def addMixedinMembers(clazz: Symbol, unit : CompilationUnit) {
    if (!(clazz hasFlag JAVA) && (treatedClassInfos get clazz) != Some(clazz.info)) {
      treatedClassInfos(clazz) = clazz.info

      assert(!clazz.isTrait, clazz)
      assert(!clazz.info.parents.isEmpty, clazz)

      // first complete the superclass with mixed in members
      addMixedinMembers(clazz.superClass,unit)

      //Console.println("adding members of " + clazz.info.baseClasses.tail.takeWhile(superclazz !=) + " to " + clazz);//DEBUG

      /** Mix in members of implementation class mixinClass into class clazz */
      def mixinImplClassMembers(impl: Symbol, iface: Symbol) {
        assert (impl.isImplClass)
        for (member <- impl.info.decls.toList) {
          if (isForwarded(member)) {
            val imember = member.overriddenSymbol(iface)
            //Console.println("mixin member "+member+":"+member.tpe+member.locationString+" "+imember+" "+imember.overridingSymbol(clazz)+" to "+clazz+" with scope "+clazz.info.decls)//DEBUG
            if (imember.overridingSymbol(clazz) == NoSymbol &&
                clazz.info.findMember(member.name, 0, lateDEFERRED, false)(NoSymbol).alternatives.contains(imember)) {
                  val member1 = addMember(
                    clazz,
                    member.cloneSymbol(clazz) setPos clazz.pos resetFlag (DEFERRED | lateDEFERRED))
                  member1.asInstanceOf[TermSymbol] setAlias member;
                }
          }
        }
      }

      /** Mix in members of trait mixinClass into class clazz. Also,
       *  for each lazy field in mixinClass, add a link from its mixed in member to it's
       *  initializer method inside the implclass.
       */
      def mixinTraitMembers(mixinClass: Symbol) {
        // For all members of a trait's interface do:
        for (member <- mixinClass.info.decls.toList) {
          if (isConcreteAccessor(member)) {
            if (isOverriddenAccessor(member, clazz.info.baseClasses)) {
              if (settings.debug.value) println("!!! is overridden val: "+member)
            } else {
              // mixin field accessors
              val member1 = addMember(
                clazz,
                member.cloneSymbol(clazz)
                  setPos clazz.pos
                  resetFlag (DEFERRED | lateDEFERRED))
              if (member.hasFlag(LAZY)) {
                var init = implClass(mixinClass).info.decl(member.name)
                assert(init != NoSymbol, "Could not find initializer for " + member.name)
                initializer(member1) = init
              }
              if (!member.isSetter)
                member.tpe match {
                  case MethodType(Nil, ConstantType(_)) =>
                    // member is a constant; only getter is needed
                    ;
                  case MethodType(Nil, TypeRef(_, UnitClass, _)) =>
                    // member is a value of type unit. No field needed
                    ;
                  case _ =>
                    // otherwise mixin a field as well
                    addMember(clazz,
                              clazz.newValue(member.pos, nme.getterToLocal(member.name))
                              setFlag (LOCAL | PRIVATE | member.getFlag(MUTABLE | LAZY))
                              setFlag (if (!member.hasFlag(STABLE)) MUTABLE else 0)
                              setInfo member.tpe.resultType
                              setAnnotations member.annotations)
                }
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

      for (mc <- clazz.mixinClasses)
        if (mc hasFlag lateINTERFACE) {
          // @SEAN: adding trait tracking so we don't have to recompile transitive closures
          unit.depends += mc
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

    case MethodType(params, restp) =>
      toInterfaceMap(
        if (isImplementedStatically(sym)) {
          val ownerParam = sym.newSyntheticValueParam(toInterface(sym.owner.typeOfThis))
          MethodType(ownerParam :: params, restp)
        } else
          tp)

    case _ =>
      tp
  }

  import scala.collection._

  /** Return a map of single-use fields to the lazy value that uses them during initialization.
   *  Each field has to be private and defined in the enclosing class, and there must
   *  be exactly one lazy value using it.
   *
   *  Such fields will be nulled after the initializer has memoized the lazy value.
   */
  def singleUseFields(templ: Template): collection.Map[Symbol, List[Symbol]] = {
    val usedIn = new mutable.HashMap[Symbol, List[Symbol]] {
      override def default(key: Symbol) = Nil
    }

    object SingleUseTraverser extends Traverser {
      override def traverse(tree: Tree) {
        tree match {
          case Assign(lhs, rhs) => traverse(rhs) // assignments don't count
          case _ =>
            if (tree.hasSymbol && tree.symbol != NoSymbol) {
              val sym = tree.symbol
              if ((sym.hasFlag(ACCESSOR) || (sym.isTerm && !sym.isMethod))
                  && sym.hasFlag(PRIVATE)
                  && !(currentOwner.isGetter && currentOwner.accessed == sym) // getter
                  && !definitions.isValueClass(sym.tpe.resultType.typeSymbol)
                  && sym.owner == templ.symbol.owner
                  && !tree.isDef) {
                log("added use in: " + currentOwner + " -- " + tree)
                usedIn(sym) ::= currentOwner

              }
            }
            super.traverse(tree)
        }
      }
    }
    SingleUseTraverser(templ)
    log("usedIn: " + usedIn)
    usedIn filter { pair =>
      val member = pair._2.head
      (member.isValue
         && member.hasFlag(LAZY)
         && pair._2.tail.isEmpty) }
  }

// --------- term transformation -----------------------------------------------

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new MixinTransformer(unit)

  class MixinTransformer(unit : CompilationUnit) extends Transformer {

    /** Within a static implementation method: the parameter referring to the
     *  current object undefined evrywhere else.
     */
    private var self: Symbol = _

    /** The rootContext used for typing */
    private val rootContext =
      erasure.NoContext.make(EmptyTree, RootClass, newScope)

    /** The typer */
    private var localTyper: erasure.Typer = _
    private def typedPos(pos: Position)(tree: Tree) = localTyper typed { atPos(pos)(tree) }

    /** Map lazy values to the fields they should null after initialization. */
    private var lazyValNullables: mutable.MultiMap[Symbol, Symbol] = _

    import scala.collection._

    /** Map a field symbol to a unique integer denoting its position in the class layout.
     *  For each class, fields defined by the class come after inherited fields. Mixed-in
     *  fields count as fields defined by the class itself.
     */
    private val fieldOffset: mutable.Map[Symbol, Int] = new mutable.HashMap[Symbol, Int]

    /** The first transform; called in a pre-order traversal at phase mixin
     *  (that is, every node is processed before its children).
     *  What transform does:
     *   - For every non-trait class, add all mixed in members to the class info.
     *   - For every trait, add all late interface members to the class info
     *   - For every static implementation method:
     *       - remove override flag
     *       - create a new method definition that also has a `self' parameter
     *         (which comes first) Iuli: this position is assumed by tail call elimination
     *         on a different receiver. Storing a new 'this' assumes it is located at
     *         index 0 in the local variable table. See 'STORE_THIS' and GenJVM/GenMSIL.
     *   - Map implementation class types in type-apply's to their interfaces
     *   - Remove all fields in implementation classes
     */
    private def preTransform(tree: Tree): Tree = {
      val sym = tree.symbol
      tree match {
        case Template(parents, self, body) =>
          localTyper = erasure.newTyper(rootContext.make(tree, currentOwner))
          atPhase(phase.next)(currentOwner.owner.info)//todo: needed?
          if (!currentOwner.isTrait) addMixedinMembers(currentOwner,unit)
          else if (currentOwner hasFlag lateINTERFACE) addLateInterfaceMembers(currentOwner)
          tree
        case DefDef(mods, name, tparams, List(vparams), tpt, rhs) if currentOwner.isImplClass =>
          if (isImplementedStatically(sym)) {
            sym setFlag notOVERRIDE
            self = sym.newValue(sym.pos, nme.SELF)
              .setFlag(PARAM)
              .setInfo(toInterface(currentOwner.typeOfThis));
            val selfdef = ValDef(self) setType NoType
            treeCopy.DefDef(tree, mods, name, tparams, List(selfdef :: vparams), tpt, rhs)
          } else {
            EmptyTree
          }
        case Apply(tapp @ TypeApply(fn, List(arg)), List()) =>
          if (arg.tpe.typeSymbol.isImplClass) {
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
    private def selfRef(pos: Position) =
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
    private def staticRef(sym: Symbol): Tree = {
      sym.owner.info  //todo: needed?
      sym.owner.owner.info //todo: needed?
      if (sym.owner.sourceModule == NoSymbol) {
        assert(false, "" + sym + " in " + sym.owner + " in " + sym.owner.owner +
                      " " + sym.owner.owner.info.decls.toList)//debug
      }
      REF(sym.owner.sourceModule) DOT sym
    }

    /** Add all new definitions to a non-trait class
     *  These fall into the following categories:
     *    - for a trait interface:
     *       - abstract accessors for all fields in the implementation class
     *    - for a non-trait class:
     *       - A field for every in a mixin class
     *       - Setters and getters for such fields
     *           - getters for mixed in lazy fields are completed
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
      def attributedDef(pos: Position, tree: Tree): Tree = {
        if (settings.debug.value) log("add new def to " + clazz + ": " + tree)
        typedPos(pos)(tree)
      }

      /** The position of given symbol, or, if this is undefined,
       *  the position of the current class. */
      def position(sym: Symbol) =
        if (sym.pos == NoPosition) clazz.pos else sym.pos

      /** Add tree at given position as new definition */
      def addDef(pos: Position, tree: Tree) {
        newDefs += attributedDef(pos, tree)
      }

      /** Add new method definition.
       *
       *  @param sym   The method
       *  @param rhs   A function that maps formal parameters to the method's
       *               right-hand side
       */
      def addDefDef(sym: Symbol, rhs: List[Symbol] => Tree) {
        addDef(position(sym), DefDef(sym, rhs(sym.paramss.head)))
      }

      /** Add `newdefs' to `stats', removing any abstract method definitions
       *  in <code>stats</code> that are matched by some symbol defined in
       *  <code>newDefs</code>.
       */
      def add(stats: List[Tree], newDefs: List[Tree]) = {
        val newSyms = newDefs map (_.symbol)
        def isNotDuplicate(tree: Tree) = tree match {
          case DefDef(_, _, _, _, _, _) =>
            val sym = tree.symbol;
            !(sym.isDeferred &&
              (newSyms exists (nsym => nsym.name == sym.name && (nsym.tpe matches sym.tpe))))
          case _ =>
            true
        }
        if (newDefs.isEmpty) stats
        else newDefs ::: stats.filter(isNotDuplicate)
      }

      /** If `stat' is a superaccessor, complete it by adding a right-hand side.
       *  Note: superaccessors are always abstract until this point.
       *   The method to call in a superaccessor is stored in the accessor symbol's alias field.
       *  The rhs is:
       *    super.A(xs)  where A is the super accessor's alias and xs are its formal parameters.
       *  This rhs is typed and then mixin transformed.
       */
      def completeSuperAccessor(stat: Tree) = stat match {
        case DefDef(mods, name, tparams, List(vparams), tpt, EmptyTree)
        if (stat.symbol hasFlag SUPERACCESSOR) =>
          val rhs0 = (Super(clazz, nme.EMPTY.toTypeName) DOT stat.symbol.alias)(vparams map (v => Ident(v.symbol)): _*)
          val rhs1 = localTyper.typed(atPos(stat.pos)(rhs0), stat.symbol.tpe.resultType)
          val rhs2 = atPhase(currentRun.mixinPhase)(transform(rhs1))
          if (settings.debug.value)
            log("complete super acc " + stat.symbol + stat.symbol.locationString +
                " " + rhs1 + " " + stat.symbol.alias + stat.symbol.alias.locationString +
                "/" + stat.symbol.alias.owner.hasFlag(lateINTERFACE))//debug
          treeCopy.DefDef(stat, mods, name, tparams, List(vparams), tpt, rhs2)
        case _ =>
          stat
      }

      import lazyVals._

      /** Return the bitmap field for 'offset', create one if not inheriting it already. */
      def bitmapFor(clazz: Symbol, offset: Int): Symbol = {
        var sym = clazz.info.member(nme.bitmapName(offset / FLAGS_PER_WORD))
        assert(!sym.hasFlag(OVERLOADED))
        if (sym == NoSymbol) {
          sym = clazz.newVariable(clazz.pos, nme.bitmapName(offset / FLAGS_PER_WORD))
                       .setInfo(IntClass.tpe)
                       .setFlag(PROTECTED)
          atPhase(currentRun.typerPhase) {
            sym addAnnotation AnnotationInfo(VolatileAttr.tpe, Nil, Nil)
          }
          clazz.info.decls.enter(sym)
          addDef(clazz.pos, VAL(sym) === ZERO)
        }
        sym
      }

      /** Return an (untyped) tree of the form 'Clazz.this.bmp = Clazz.this.bmp | mask'. */
      def mkSetFlag(clazz: Symbol, offset: Int): Tree = {
        val bmp   = bitmapFor(clazz, offset)
        val mask  = LIT(1 << (offset % FLAGS_PER_WORD))
        def x     = This(clazz) DOT bmp

        x === (x INT_| mask)
      }

      /** Return an (untyped) tree of the form 'clazz.this.bitmapSym & mask (==|!=) 0', the
       *  precise comparison operator depending on the value of 'equalToZero'.
       */
      def mkTest(clazz: Symbol, mask: Tree, bitmapSym: Symbol, equalToZero: Boolean): Tree = {
        def lhs = (This(clazz) DOT bitmapSym) INT_& mask
        if (equalToZero)  lhs INT_== ZERO
        else              lhs INT_!= ZERO
      }

      /** return a 'lazified' version of rhs.
       *  @param clazz The class symbol
       *  @param init The tree which initializes the field ( f = <rhs> )
       *  @param fieldSym The symbol of this lazy field
       *  @param offset The offset of this field in the flags bitmap
       *
       *  The result will be a tree of the form
       *  {
       *    if ((bitmap$n & MASK) == 0) {
       *       synhronized(this) {
       *         if ((bitmap$n & MASK) == 0) {
       *           synhronized(this) {
       *             init // l$ = <rhs>
       *           }
       *           bitmap$n = bimap$n | MASK
       *         }}}
       *    l$
       *  }
       *  where bitmap$n is an int value acting as a bitmap of initialized values. It is
       *  the 'n' is (offset / 32), the MASK is (1 << (offset % 32)).
       */
      def mkLazyDef(clazz: Symbol, lzyVal: Symbol, init: List[Tree], retVal: Tree, offset: Int): Tree = {
        def nullify(sym: Symbol): Tree = {
          val sym1 = if (sym.hasFlag(ACCESSOR)) sym.accessed else sym
          Select(This(clazz), sym1) === LIT(null)
        }


        val bitmapSym = bitmapFor(clazz, offset)
        val mask      = LIT(1 << (offset % FLAGS_PER_WORD))
        def cond      = mkTest(clazz, mask, bitmapSym, true)
        val nulls     = (lazyValNullables(lzyVal).toList.sort(_.id < _.id) map nullify)
        def syncBody  = init ::: List(mkSetFlag(clazz, offset), UNIT)

        log("nulling fields inside " + lzyVal + ": " + nulls)
        val result    =
          IF (cond) THEN BLOCK(
            (gen.mkSynchronized(
              gen mkAttributedThis clazz,
              IF (cond) THEN BLOCK(syncBody: _*) ENDIF
            )
            :: nulls): _*) ENDIF

        typedPos(init.head.pos)(BLOCK(result, retVal))
      }

      def mkCheckedAccessor(clazz: Symbol, retVal: Tree, offset: Int, pos: Position): Tree = {
        val bitmapSym = bitmapFor(clazz, offset)
        val mask      = LIT(1 << (offset % FLAGS_PER_WORD))
        val msg       = "Uninitialized field: " + unit.source + ": " + pos.line
        val result    =
          IF (mkTest(clazz, mask, bitmapSym, false)) .
            THEN (retVal) .
            ELSE (THROW(UninitializedErrorClass, LIT(msg)))

        typedPos(pos)(BLOCK(result, retVal))
      }

      /** Complete lazy field accessors. Applies only to classes, for it's own (non inherited) lazy fields.
       *  If 'checkinit' is enabled, getters that check for the initialized bit are generated, and
       *  the class constructor is changed to set the initialized bits.
       */
      def addCheckedGetters(clazz: Symbol, stats: List[Tree]): List[Tree] = {
        def findLazyAssignment(stats: List[Tree]): Tree = (
          for (s @ Assign(lhs, _) <- stats ; if lhs.symbol hasFlag LAZY) yield s
        ) head // if there's no assignment then it's a bug and we crash

        val stats1 = for (stat <- stats; sym = stat.symbol) yield stat match {
          case DefDef(mods, name, tp, vp, tpt, rhs)
            if sym.hasFlag(LAZY) && rhs != EmptyTree && !clazz.isImplClass =>
              assert(fieldOffset.isDefinedAt(sym))
              val rhs1 = if (sym.tpe.resultType.typeSymbol == UnitClass)
                mkLazyDef(clazz, sym, List(rhs), UNIT, fieldOffset(sym))
              else {
                val Block(stats, res) = rhs
                mkLazyDef(clazz, sym, stats, Select(This(clazz), res.symbol), fieldOffset(sym))
              }
              treeCopy.DefDef(stat, mods, name, tp, vp, tpt, rhs1)

          case DefDef(mods, name, tp, vp, tpt, rhs)
            if needsInitFlag(sym) && rhs != EmptyTree && !clazz.isImplClass && !clazz.isTrait =>
              assert(fieldOffset.isDefinedAt(sym))

              val rhs1 = (mkCheckedAccessor(clazz, _: Tree, fieldOffset(sym), stat.pos))(
                if (sym.tpe.resultType.typeSymbol == UnitClass) UNIT else rhs
              )
              treeCopy.DefDef(stat, mods, name, tp, vp, tpt, rhs1)

          case DefDef(mods, name, tp, vp, tpt, rhs) if sym.isConstructor =>
            treeCopy.DefDef(stat, mods, name, tp, vp, tpt, addInitBits(clazz, rhs))

          case _ => stat
        }
        stats1
      }

      /** Does this field require an intialized bit? */
      def needsInitFlag(sym: Symbol) = {
        val res = (settings.checkInit.value
           && sym.isGetter
           && !sym.isInitializedToDefault
           && !sym.hasFlag(PARAMACCESSOR)
           && !sym.accessed.hasFlag(PRESUPER)
           && !sym.isOuterAccessor)

        if (settings.debug.value) {
          log("needsInitFlag(" + sym.fullNameString + "): " + res)
          log("\tsym.isGetter: " + sym.isGetter)
          log("\t!isInitializedToDefault: " + !sym.isInitializedToDefault + sym.hasFlag(DEFAULTINIT) + sym.hasFlag(ACCESSOR) + sym.isTerm)
          log("\t!sym.hasFlag(PARAMACCESSOR): " + !sym.hasFlag(PARAMACCESSOR))
          //println("\t!sym.accessed.hasFlag(PRESUPER): " + !sym.accessed.hasFlag(PRESUPER))
          log("\t!sym.isOuterAccessor: " + !sym.isOuterAccessor)
        }

        res
      }

      /** Adds statements to set the 'init' bit for each field initialized
       * in the body of a constructor.
       */
      def addInitBits(clazz: Symbol, rhs: Tree): Tree = {
        new Transformer {
          override def transformStats(stats: List[Tree], exprOwner: Symbol) = {
            val stats1 = stats flatMap { stat => stat match {
              case Assign(lhs @ Select(This(_), _), rhs) =>
                val sym = clazz.info.decl(nme.getterName(lhs.symbol.name))
                  .suchThat(_.isGetter)
                if (rhs == EmptyTree)
                  List()
                else if (sym != NoSymbol && needsInitFlag(sym) && fieldOffset.isDefinedAt(sym)) {
                  log("adding checked getter for: " + sym + " " + Flags.flagsToString(lhs.symbol.flags))
                  List(stat, localTyper.typed(mkSetFlag(clazz, fieldOffset(sym))))
                } else {
                  List(stat)
                }
              case Apply(setter @ Select(Ident(self), _), List(EmptyTree)) if setter.symbol.isSetter =>
                // remove initialization for default values
                List()
              case _ => List(stat)
            }
            }
            super.transformStats(stats1, exprOwner)
          }
        }.transform(rhs)
      }

      /** Return the number of bits used by superclass fields.
       *  This number is a conservative approximation of what is actually used:
       *    - fields initialized to the default value don't get a checked initializer
       *      but there is no way to recover this information from types alone.
       */
      def usedBits(clazz: Symbol): Int = {
        def isField(sym: Symbol) =
          sym.hasFlag(PRIVATE) && sym.isTerm && !sym.isMethod

        def needsBitmapField(sc: Type, field: Symbol) =
          !sc.typeSymbol.isTrait &&
          field.owner != clazz &&
          (settings.checkInit.value && isField(field) ||
            field.hasFlag(LAZY))

        // parents != baseClasses.map(_.tpe): bug #1535
        val fields = for {
          sc <- clazz.info.baseClasses.map(_.tpe)
          field <- sc.decls.iterator.toList
          if needsBitmapField(sc, field)
        } yield field

        if (settings.debug.value) log("Found inherited fields in " + clazz + " : " + fields)
        fields.length
      }


      /** Fill the map from fields to offset numbers.
       *  Instead of field symbols, the map keeps their getter symbols. This makes
       *  code generation easier later.
       */
      def buildFieldPositions(clazz: Symbol) {
        var fields = usedBits(clazz)
        for (f <- clazz.info.decls.iterator if needsInitFlag(f) || f.hasFlag(LAZY)) {
          if (settings.debug.value) log(f.fullNameString + " -> " + fields)
          fieldOffset(f) = fields
          fields += 1
        }
      }

      buildFieldPositions(clazz)
      // begin addNewDefs
      var stats1 = addCheckedGetters(clazz, stats)

      // for all symbols `sym' in the class definition, which are mixed in:
      for (sym <- clazz.info.decls.toList) {
        if (sym hasFlag MIXEDIN) {
          if (clazz hasFlag lateINTERFACE) {
            // if current class is a trait interface, add an abstract method for accessor `sym'
            addDefDef(sym, vparamss => EmptyTree)
          } else if (!clazz.isTrait) {
            // if class is not a trait add accessor definitions
            if ((sym hasFlag ACCESSOR) &&
                (!(sym hasFlag DEFERRED) || (sym hasFlag lateDEFERRED))) {
              // add accessor definitions
              addDefDef(sym, vparams => {
                val accessedRef = sym.tpe match {
                  case MethodType(List(), ConstantType(c)) => Literal(c)
                  case _ =>
                    // if it is a mixed-in lazy value, complete the accessor
                    if (sym.hasFlag(LAZY) && sym.isGetter) {
                      val rhs1 =
                        if (sym.tpe.resultType.typeSymbol == UnitClass)
                          mkLazyDef(clazz, sym, List(Apply(staticRef(initializer(sym)), List(gen.mkAttributedThis(clazz)))), UNIT, fieldOffset(sym))
                        else {
                          val assign = atPos(sym.pos) {
                            Assign(Select(This(sym.accessed.owner), sym.accessed) /*gen.mkAttributedRef(sym.accessed)*/ ,
                                Apply(staticRef(initializer(sym)), gen.mkAttributedThis(clazz) :: Nil))
                          }
                          mkLazyDef(clazz, sym, List(assign), Select(This(clazz), sym.accessed), fieldOffset(sym))
                        }
                      rhs1
                    } else if (sym.getter(sym.owner).tpe.resultType.typeSymbol == UnitClass) {
                      UNIT
                    } else {
                      Select(This(clazz), sym.accessed)
                    }
                }
                if (sym.isSetter) {
                  val isOverriddenSetter =
                    nme.isTraitSetterName(sym.name) && {
                      sym.allOverriddenSymbols match {
                        case other :: _ =>
                          isOverriddenAccessor(other.getter(other.owner), clazz.info.baseClasses)
                        case _ =>
                          false
                      }
                    }
                  if (isOverriddenSetter) UNIT
                  else accessedRef match {
                    case Literal(_) => accessedRef
                    case _ =>
                      val init = Assign(accessedRef, Ident(vparams.head))
                      if (settings.checkInit.value && needsInitFlag(sym.getter(clazz))) {
                        Block(List(init, mkSetFlag(clazz, fieldOffset(sym.getter(clazz)))), UNIT)
                      } else
                        init
                  }
                } else if (!sym.hasFlag(LAZY) && needsInitFlag(sym)) {
                  mkCheckedAccessor(clazz,  accessedRef, fieldOffset(sym), sym.pos)
                } else
                  gen.mkCheckInit(accessedRef)
              })
            } else if (sym.isModule && !(sym hasFlag LIFTED | BRIDGE)) {
              // add modules
              val vdef = gen.mkModuleVarDef(sym)
              addDef(position(sym), vdef)
              addDef(position(sym), gen.mkCachedModuleAccessDef(sym, vdef.symbol))
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
      stats1 = add(stats1, newDefs.toList)
      if (!clazz.isTrait) stats1 = stats1 map completeSuperAccessor
      stats1
    }

    private def nullableFields(templ: Template) = {
      val nullables = new mutable.HashMap[Symbol, mutable.Set[Symbol]] with mutable.MultiMap[Symbol, Symbol] {
        override def default(key: Symbol) = mutable.Set.empty
      }

      // if there are no lazy fields, take the fast path and save a traversal of the whole AST
      if (templ.symbol.owner.info.decls.exists(_.hasFlag(LAZY))) {
        // check what fields can be nulled for
        val uses = singleUseFields(templ)
        for ((field, users) <- uses; lazyFld <- users) {
          nullables.addBinding(lazyFld, field)
        }
      }
      nullables
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
      if (tree.tpe.typeSymbol.isImplClass &&
          ((tree.symbol eq null) || !tree.symbol.isImplClass))
        tree.tpe = toInterface(tree.tpe);

      tree match {
        case Template(parents, self, body) =>
          // change parents of templates to conform to parents in the symbol info
          val parents1 = currentOwner.info.parents map (t => TypeTree(t) setPos tree.pos)

          lazyValNullables = nullableFields(tree.asInstanceOf[Template])
          // add all new definitions to current class or interface
          val body1 = addNewDefs(currentOwner, body)

          treeCopy.Template(tree, parents1, self, body1)

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

            typedPos(tree.pos)(Apply(staticRef(target), transformSuper(qual) :: args))
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
                  typedPos(tree.pos)((transformSuper(qual) DOT sym1)())
                }
                else {
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
          assert(!sym.isMethod, "no method allowed here: %s%s %s".format(sym, sym.isImplOnly, flagsToString(sym.flags)))

          // refer to fields in some implementation class via an abstract
          // getter in the interface.
          val iface = toInterface(sym.owner.tpe).typeSymbol
          val getter = sym.getter(iface)
          assert(getter != NoSymbol)
          typedPos(tree.pos)((qual DOT getter)())

        case Assign(Apply(lhs @ Select(qual, _), List()), rhs) =>
          // assign to fields in some implementation class via an abstract
          // setter in the interface.
          def setter = lhs.symbol.setter(
            toInterface(lhs.symbol.owner.tpe).typeSymbol,
            needsExpandedSetterName(lhs.symbol)
          ) setPos lhs.pos

          typedPos(tree.pos) { (qual DOT setter)(rhs) }

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
        val outerTyper = localTyper
        val tree1 = super.transform(preTransform(tree))
        val res = atPhase(phase.next)(postTransform(tree1))
        // needed when not flattening inner classes. parts after an
        // inner class will otherwise be typechecked with a wrong scope
        localTyper = outerTyper
        res
      } catch {
        case ex: Throwable =>
          if (settings.debug.value) Console.println("exception when traversing " + tree)
          throw ex
      }
    }
  }
}
