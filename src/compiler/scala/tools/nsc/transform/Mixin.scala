/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package scala.tools.nsc
package transform

import symtab._
import Flags._
import scala.annotation.tailrec
import scala.collection.mutable

abstract class Mixin extends InfoTransform with ast.TreeDSL {
  import global._
  import definitions._
  import CODE._

  /** The name of the phase: */
  val phaseName: String = "mixin"

  /** The phase might set the following new flags: */
  override def phaseNewFlags: Long = lateMODULE | notOVERRIDE

  /** This map contains a binding (class -> info) if
   *  the class with this info at phase mixinPhase has been treated for mixin composition
   */
  private val treatedClassInfos = perRunCaches.newMap[Symbol, Type]() withDefaultValue NoType

  /** Map a lazy, mixedin field accessor to its trait member accessor */
  private val initializer = perRunCaches.newMap[Symbol, Symbol]()

// --------- helper functions -----------------------------------------------

  /** A member of a trait is implemented statically if its implementation after the
   *  mixin transform is RHS of the method body (destined to be in a interface default method)
   *
   *  To be statically implemented, a member must be a method that belonged to the trait's implementation class
   *  before (i.e. it is not abstract). Not statically implemented are
   *   - non-private modules: these are implemented directly in the mixin composition class
   *     (private modules, on the other hand, are implemented statically, but their
   *      module variable is not. all such private modules are lifted, because
   *      non-lifted private modules have been eliminated in ExplicitOuter)
   *   - field accessors and superaccessors, except for lazy value accessors which become initializer
   *     methods in the impl class (because they can have arbitrary initializers)
   */
  private def isImplementedStatically(sym: Symbol) = (
       sym.isMethod
    && (!sym.hasFlag(DEFERRED | SUPERACCESSOR) || (sym hasFlag lateDEFERRED))
    && sym.owner.isTrait
    && (!sym.isModule || sym.hasFlag(PRIVATE | LIFTED))
    && (!(sym hasFlag (ACCESSOR | SUPERACCESSOR)) || sym.isLazy)
    && !sym.isPrivate
    && !sym.hasAllFlags(LIFTED | MODULE | METHOD)
    && !sym.isConstructor
    && (!sym.hasFlag(notPRIVATE | LIFTED) || sym.hasFlag(ACCESSOR | SUPERACCESSOR | MODULE))
  )

  private def isFieldWithBitmap(field: Symbol) = {
    field.info // ensure that nested objects are transformed
    // For checkinit consider normal value getters
    // but for lazy values only take into account lazy getters
    field.isLazy && field.isMethod && !field.isDeferred
  }

  /** Does this field require an initialized bit?
   *  Note: fields of classes inheriting DelayedInit are not checked.
   *        This is because they are neither initialized in the constructor
   *        nor do they have a setter (not if they are vals anyway). The usual
   *        logic for setting bitmaps does therefore not work for such fields.
   *        That's why they are excluded.
   *  Note: The `checkinit` option does not check if transient fields are initialized.
   */
  private def needsInitFlag(sym: Symbol) = (
        settings.checkInit
     && sym.isGetter
     && !sym.isInitializedToDefault
     && !isConstantType(sym.info.finalResultType) // SI-4742
     && !sym.hasFlag(PARAMACCESSOR | SPECIALIZED | LAZY)
     && !sym.accessed.hasFlag(PRESUPER)
     && !sym.isOuterAccessor
     && !(sym.owner isSubClass DelayedInitClass)
     && !(sym.accessed hasAnnotation TransientAttr)
  )

  /** Returns the symbol that is accessed by a super-accessor in a mixin composition.
   *
   *  @param base       The class in which everything is mixed together
   *  @param member     The symbol statically referred to by the superaccessor in the trait
   *  @param mixinClass The mixin class that produced the superaccessor
   */
  private def rebindSuper(base: Symbol, member: Symbol, mixinClass: Symbol): Symbol =
    exitingSpecialize {
      var bcs = base.info.baseClasses.dropWhile(mixinClass != _).tail
      var sym: Symbol = NoSymbol
      debuglog("starting rebindsuper " + base + " " + member + ":" + member.tpe +
            " " + mixinClass + " " + base.info.baseClasses + "/" + bcs)
      while (!bcs.isEmpty && sym == NoSymbol) {
        if (settings.debug) {
          val other = bcs.head.info.nonPrivateDecl(member.name)
          debuglog("rebindsuper " + bcs.head + " " + other + " " + other.tpe +
              " " + other.isDeferred)
        }
        sym = member.matchingSymbol(bcs.head, base.thisType).suchThat(sym => !sym.hasFlag(DEFERRED | BRIDGE))
        bcs = bcs.tail
      }
      sym
    }

// --------- type transformation -----------------------------------------------

  def isConcreteAccessor(member: Symbol) =
    member.hasAccessorFlag && (!member.isDeferred || (member hasFlag lateDEFERRED))

  /** Is member overridden (either directly or via a bridge) in base class sequence `bcs`? */
  def isOverriddenAccessor(member: Symbol, bcs: List[Symbol]): Boolean = beforeOwnPhase {
    def hasOverridingAccessor(clazz: Symbol) = {
      clazz.info.nonPrivateDecl(member.name).alternatives.exists(
        sym =>
          isConcreteAccessor(sym) &&
          !sym.hasFlag(MIXEDIN) &&
          matchesType(sym.tpe, member.tpe, alwaysMatchSimple = true))
    }
    (    bcs.head != member.owner
      && (hasOverridingAccessor(bcs.head) || isOverriddenAccessor(member, bcs.tail))
    )
  }

  /** Add given member to given class, and mark member as mixed-in.
   */
  def addMember(clazz: Symbol, member: Symbol): Symbol = {
    debuglog(s"mixing into $clazz: ${member.defString}")
    // This attachment is used to instruct the backend about which methids in traits require
    // a static trait impl method. We remove this from the new symbol created for the method
    // mixed into the subclass.
    member.removeAttachment[NeedStaticImpl.type]
    clazz.info.decls enter member setFlag MIXEDIN resetFlag JAVA_DEFAULTMETHOD
  }
  def cloneAndAddMember(mixinClass: Symbol, mixinMember: Symbol, clazz: Symbol): Symbol =
    addMember(clazz, cloneBeforeErasure(mixinClass, mixinMember, clazz))

  def cloneBeforeErasure(mixinClass: Symbol, mixinMember: Symbol, clazz: Symbol): Symbol = {
    val newSym = enteringErasure {
      // since we used `mixinMember` from the interface that represents the trait that's
      // being mixed in, have to instantiate the interface type params (that may occur in mixinMember's
      // info) as they are seen from the class.  We can't use the member that we get from the
      // implementation class, as it's a clone that was made after erasure, and thus it does not
      // know its info at the beginning of erasure anymore.
      val sym = mixinMember cloneSymbol clazz

      val erasureMap = erasure.erasure(mixinMember)
      val erasedInterfaceInfo: Type = erasureMap(mixinMember.info)
      val specificForwardInfo       = (clazz.thisType baseType mixinClass) memberInfo mixinMember
      val forwarderInfo =
        if (erasureMap(specificForwardInfo) =:= erasedInterfaceInfo)
          specificForwardInfo
        else {
          erasedInterfaceInfo
        }
      // Optimize: no need if mixinClass has no typeparams.
      // !!! JZ Really? What about the effect of abstract types, prefix?
      if (mixinClass.typeParams.isEmpty) sym
      else sym modifyInfo (_ => forwarderInfo)
    }
    newSym
  }

  /** Add getters and setters for all non-module fields of an implementation
   *  class to its interface unless they are already present. This is done
   *  only once per class. The mixedin flag is used to remember whether late
   *  members have been added to an interface.
   *    - lazy fields don't get a setter.
   */
  def addLateInterfaceMembers(clazz: Symbol) {
    if (treatedClassInfos(clazz) != clazz.info) {
      treatedClassInfos(clazz) = clazz.info
      assert(phase == currentRun.mixinPhase, phase)

      /* Create a new getter. Getters are never private or local. They are
       *  always accessors and deferred. */
      def newGetter(field: Symbol): Symbol = {
        // println("creating new getter for "+ field +" : "+ field.info +" at "+ field.locationString+(field hasFlag MUTABLE))
        val newFlags = field.flags & ~PrivateLocal | ACCESSOR | lateDEFERRED | ( if (field.isMutable) 0 else STABLE )
        // TODO preserve pre-erasure info?
        clazz.newMethod(field.getterName, field.pos, newFlags) setInfo MethodType(Nil, field.info)
      }

      /* Create a new setter. Setters are never private or local. They are
       * always accessors and deferred. */
      def newSetter(field: Symbol): Symbol = {
        //println("creating new setter for "+field+field.locationString+(field hasFlag MUTABLE))
        val setterName = field.setterName
        val newFlags   = field.flags & ~PrivateLocal | ACCESSOR | lateDEFERRED
        val setter     = clazz.newMethod(setterName, field.pos, newFlags)
        // TODO preserve pre-erasure info?
        setter setInfo MethodType(setter.newSyntheticValueParams(List(field.info)), UnitTpe)
        if (field.needsExpandedSetterName)
          setter.name = nme.expandedSetterName(setter.name, clazz)

        setter
      }

      clazz.info // make sure info is up to date, so that implClass is set.

      for (member <- clazz.info.decls) {
        if (!member.isMethod && !member.isModule && !member.isModuleVar) {
          assert(member.isTerm && !member.isDeferred, member)
          if (member.getterIn(clazz).isPrivate) {
            member.makeNotPrivate(clazz) // this will also make getter&setter not private
          }
          val getter = member.getterIn(clazz)
          if (getter == NoSymbol) addMember(clazz, newGetter(member))
          if (!member.tpe.isInstanceOf[ConstantType] && !member.isLazy) {
            val setter = member.setterIn(clazz)
            if (setter == NoSymbol) addMember(clazz, newSetter(member))
          }
          clazz.info.decls.unlink(member)
        }
      }
      debuglog("new defs of " + clazz + " = " + clazz.info.decls)
    }
  }

  /** Add all members to be mixed in into a (non-trait-) class
   *  These are:
   *    for every mixin trait T that is not also inherited by the superclass:
   *     add late interface members to T and then:
   *      - if a member M of T is forwarded to the implementation class, add
   *        a forwarder for M unless one exists already.
   *        The alias of the forwarder is the static member it forwards to.
   *      - for every abstract accessor in T, add a field and an implementation for that accessor
   *      - for every super accessor in T, add an implementation of that accessor
   *      - for every module in T, add a module
   */
  def addMixedinMembers(clazz: Symbol, unit: CompilationUnit) {
    def cloneAndAddMixinMember(mixinClass: Symbol, mixinMember: Symbol): Symbol = (
      cloneAndAddMember(mixinClass, mixinMember, clazz)
           setPos clazz.pos
        resetFlag DEFERRED | lateDEFERRED
    )

    /* Mix in members of implementation class mixinClass into class clazz */
    def mixinTraitForwarders(mixinClass: Symbol) {
      for (member <- mixinClass.info.decls ; if isImplementedStatically(member)) {
        member overridingSymbol clazz match {
          case NoSymbol =>
            val isMemberOfClazz = clazz.info.findMember(member.name, 0, 0L, stableOnly = false).alternatives.contains(member)
            if (isMemberOfClazz) {
              // `member` is a concrete method defined in `mixinClass`, which is a base class of
              // `clazz`, and the method is not overridden in `clazz`. A forwarder is needed if:
              //
              //   - A non-trait base class of `clazz` defines a matching method. Example:
              //       class C {def f: Int}; trait T extends C {def f = 1}; class D extends T
              //     Even if C.f is abstract, the forwarder in D is needed, otherwise the JVM would
              //     resolve `D.f` to `C.f`, see jvms-6.5.invokevirtual.
              //
              //   - There exists another concrete, matching method in a parent interface `p` of
              //     `clazz`, and the `mixinClass` does not itself extend `p`. In this case the
              //     forwarder is needed to disambiguate. Example:
              //       trait T1 {def f = 1}; trait T2 extends T1 {override def f = 2}; class C extends T2
              //     In C we don't need a forwarder for f because T2 extends T1, so the JVM resolves
              //     C.f to T2.f non-ambiguously. See jvms-5.4.3.3, "maximally-specific method".
              //       trait U1 {def f = 1}; trait U2 {self:U1 => override def f = 2}; class D extends U2
              //     In D the forwarder is needed, the interfaces U1 and U2 are unrelated at the JVM
              //     level.

              @tailrec
              def existsCompetingMethod(baseClasses: List[Symbol]): Boolean = baseClasses match {
                case baseClass :: rest =>
                  if (baseClass ne mixinClass) {
                    val m = member.overriddenSymbol(baseClass)
                    val isCompeting = m.exists && {
                      !m.owner.isTraitOrInterface ||
                        (!m.isDeferred && !mixinClass.isNonBottomSubClass(m.owner))
                    }
                    isCompeting || existsCompetingMethod(rest)
                  } else existsCompetingMethod(rest)

                case _ => false
              }

              if (existsCompetingMethod(clazz.baseClasses))
                cloneAndAddMixinMember(mixinClass, member).asInstanceOf[TermSymbol] setAlias member
              else if (!settings.nowarnDefaultJunitMethods && JUnitTestClass.exists && member.hasAnnotation(JUnitTestClass))
                warning(member.pos, "JUnit tests in traits that are compiled as default methods are not executed by JUnit 4. JUnit 5 will fix this issue.")
            }

          case _        =>
        }
      }
    }

    /* Mix in members of trait mixinClass into class clazz. Also,
     * for each lazy field in mixinClass, add a link from its mixed in member to its
     * initializer method inside the implclass.
     */
    def mixinTraitMembers(mixinClass: Symbol) {
      // For all members of a trait's interface do:
      for (mixinMember <- mixinClass.info.decls) {
        if (isConcreteAccessor(mixinMember)) {
          if (isOverriddenAccessor(mixinMember, clazz.info.baseClasses))
            devWarning(s"Overridden concrete accessor: ${mixinMember.fullLocationString}")
          else {
            // mixin field accessors
            val mixedInAccessor = cloneAndAddMixinMember(mixinClass, mixinMember)
            if (mixinMember.isLazy) {
              initializer(mixedInAccessor) = (
                mixinClass.info.decl(mixinMember.name)
                  orElse abort("Could not find initializer for " + mixinMember.name)
              )
            }
            if (!mixinMember.isSetter)
              mixinMember.tpe match {
                case MethodType(Nil, ConstantType(_)) =>
                  // mixinMember is a constant; only getter is needed
                  ;
                case MethodType(Nil, TypeRef(_, UnitClass, _)) =>
                  // mixinMember is a value of type unit. No field needed
                  ;
                case _ => // otherwise mixin a field as well
                  // enteringPhase: the private field is moved to the implementation class by erasure,
                  // so it can no longer be found in the mixinMember's owner (the trait)
                  val accessed = enteringPickler(mixinMember.accessed)
                  // #3857, need to retain info before erasure when cloning (since cloning only
                  // carries over the current entry in the type history)
                  val sym = enteringErasure {
                    // so we have a type history entry before erasure
                    clazz.newValue(mixinMember.localName, mixinMember.pos).setInfo(mixinMember.tpe.resultType)
                  }
                  sym updateInfo mixinMember.tpe.resultType // info at current phase

                  val newFlags = (
                      ( PrivateLocal )
                    | ( mixinMember getFlag MUTABLE | LAZY)
                    | ( if (mixinMember.hasStableFlag) 0 else MUTABLE )
                  )

                  addMember(clazz, sym setFlag newFlags setAnnotations accessed.annotations)
              }
          }
        }
        else if (mixinMember.isSuperAccessor) { // mixin super accessors
          val superAccessor = addMember(clazz, mixinMember.cloneSymbol(clazz)) setPos clazz.pos
          assert(superAccessor.alias != NoSymbol, superAccessor)

          rebindSuper(clazz, mixinMember.alias, mixinClass) match {
            case NoSymbol =>
              reporter.error(clazz.pos, "Member %s of mixin %s is missing a concrete super implementation.".format(
                mixinMember.alias, mixinClass))
            case alias1 =>
              if (alias1.owner.isJavaDefined && alias1.owner.isInterface && !clazz.parentSymbols.contains(alias1.owner)) {
                val suggestedParent = exitingTyper(clazz.info.baseType(alias1.owner))
                reporter.error(clazz.pos, s"Unable to implement a super accessor required by trait ${mixinClass.name} unless $suggestedParent is directly extended by $clazz.")
              }
              superAccessor.asInstanceOf[TermSymbol] setAlias alias1
          }
        }
        else if (mixinMember.isMethod && mixinMember.isModule && mixinMember.hasNoFlags(LIFTED | BRIDGE)) {
          // mixin objects: todo what happens with abstract objects?
          addMember(clazz, mixinMember.cloneSymbol(clazz, mixinMember.flags & ~(DEFERRED | lateDEFERRED)) setPos clazz.pos)
        }
      }
    }

    if (clazz.isJavaDefined || treatedClassInfos(clazz) == clazz.info)
      return

    treatedClassInfos(clazz) = clazz.info
    assert(!clazz.isTrait && clazz.info.parents.nonEmpty, clazz)

    // first complete the superclass with mixed in members
    addMixedinMembers(clazz.superClass, unit)

    for (mc <- clazz.mixinClasses ; if mc.isTrait) {
      // @SEAN: adding trait tracking so we don't have to recompile transitive closures
      unit.depends += mc
      addLateInterfaceMembers(mc)
      mixinTraitMembers(mc)
      mixinTraitForwarders(mc)
    }
  }

  override def transformInfo(sym: Symbol, tp: Type): Type = tp

  /** Return a map of single-use fields to the lazy value that uses them during initialization.
   *  Each field has to be private and defined in the enclosing class, and there must
   *  be exactly one lazy value using it.
   *
   *  Such fields will be nulled after the initializer has memoized the lazy value.
   */
  def singleUseFields(templ: Template): scala.collection.Map[Symbol, List[Symbol]] = {
    val usedIn = mutable.HashMap[Symbol, List[Symbol]]() withDefaultValue Nil

    object SingleUseTraverser extends Traverser {
      override def traverse(tree: Tree) {
        tree match {
          case Assign(lhs, rhs) => traverse(rhs) // assignments don't count
          case _ =>
            if (tree.hasSymbolField && tree.symbol != NoSymbol) {
              val sym = tree.symbol
              if ((sym.hasAccessorFlag || (sym.isTerm && !sym.isMethod))
                  && sym.isPrivate
                  && !(currentOwner.isGetter && currentOwner.accessed == sym) // getter
                  && !definitions.isPrimitiveValueClass(sym.tpe.resultType.typeSymbol)
                  && sym.owner == templ.symbol.owner
                  && !sym.isLazy
                  && !tree.isDef) {
                debuglog("added use in: " + currentOwner + " -- " + tree)
                usedIn(sym) ::= currentOwner

              }
            }
            super.traverse(tree)
        }
      }
    }
    SingleUseTraverser(templ)
    debuglog("usedIn: " + usedIn)
    usedIn filter {
      case (_, member :: Nil) => member.isValue && member.isLazy
      case _                  => false
    }
  }

// --------- term transformation -----------------------------------------------

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new MixinTransformer(unit)

  class MixinTransformer(unit : CompilationUnit) extends Transformer {

    /** The rootContext used for typing */
    private val rootContext =
      erasure.NoContext.make(EmptyTree, rootMirror.RootClass, newScope)

    /** The typer */
    private var localTyper: erasure.Typer = _
    private def typedPos(pos: Position)(tree: Tree): Tree = localTyper.typedPos(pos)(tree)

    /** Map lazy values to the fields they should null after initialization. */
    private var lazyValNullables: Map[Symbol, Set[Symbol]] = _

    /** Map a field symbol to a unique integer denoting its position in the class layout.
     *  For each class, fields defined by the class come after inherited fields. Mixed-in
     *  fields count as fields defined by the class itself.
     */
    private val fieldOffset = perRunCaches.newMap[Symbol, Int]()

    private val bitmapKindForCategory = perRunCaches.newMap[Name, ClassSymbol]()

    // ByteClass, IntClass, LongClass
    private def bitmapKind(field: Symbol): ClassSymbol = bitmapKindForCategory(bitmapCategory(field))

    private def flagsPerBitmap(field: Symbol): Int = bitmapKind(field) match {
      case BooleanClass => 1
      case ByteClass    => 8
      case IntClass     => 32
      case LongClass    => 64
    }


    /** The first transform; called in a pre-order traversal at phase mixin
     *  (that is, every node is processed before its children).
     *  What transform does:
     *   - For every non-trait class, add all mixed in members to the class info.
     */
    private def preTransform(tree: Tree): Tree = {
      val sym = tree.symbol
      tree match {
        case Template(parents, self, body) =>
          localTyper = erasure.newTyper(rootContext.make(tree, currentOwner))
          exitingMixin(currentOwner.owner.info)//todo: needed?

          if (!currentOwner.isTrait && !isPrimitiveValueClass(currentOwner))
            addMixedinMembers(currentOwner, unit)
          else if (currentOwner.isTrait)
            addLateInterfaceMembers(currentOwner)

          tree

        case _ =>
          if (currentOwner.isTrait && sym.isSetter && !enteringPickler(sym.isDeferred)) {
            sym.addAnnotation(TraitSetterAnnotationClass)
          }
          tree
      }
    }

    def needsInitAndHasOffset(sym: Symbol) =
      needsInitFlag(sym) && (fieldOffset contains sym)

    /** Examines the symbol and returns a name indicating what brand of
     *  bitmap it requires.  The possibilities are the BITMAP_* vals
     *  defined in StdNames.  If it needs no bitmap, nme.NO_NAME.
     */
    def bitmapCategory(field: Symbol): Name = {
      import nme._
      val isNormal = (
        if (isFieldWithBitmap(field)) true
        // bitmaps for checkinit fields are not inherited
        else if (needsInitFlag(field) && !field.isDeferred) false
        else return NO_NAME
      )
      if (field.accessed hasAnnotation TransientAttr) {
        if (isNormal) BITMAP_TRANSIENT
        else BITMAP_CHECKINIT_TRANSIENT
      } else {
        if (isNormal) BITMAP_NORMAL
        else BITMAP_CHECKINIT
      }
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
     *
     *  @param clazz  The class to which definitions are added
     */
    private def addNewDefs(clazz: Symbol, stats: List[Tree]): List[Tree] = {
      val newDefs = mutable.ListBuffer[Tree]()

      /* Attribute given tree and anchor at given position */
      def attributedDef(pos: Position, tree: Tree): Tree = {
        debuglog("add new def to " + clazz + ": " + tree)
        typedPos(pos)(tree)
      }

      /* The position of given symbol, or, if this is undefined,
       * the position of the current class.
       */
      def position(sym: Symbol) =
        if (sym.pos == NoPosition) clazz.pos else sym.pos

      /* Add tree at given position as new definition */
      def addDef(pos: Position, tree: Tree) {
        newDefs += attributedDef(pos, tree)
      }

      /* Add new method definition.
       *
       * @param sym   The method symbol.
       * @param rhs   The method body.
       */
      def addDefDef(sym: Symbol, rhs: Tree = EmptyTree) = addDef(position(sym), DefDef(sym, rhs))
      def addValDef(sym: Symbol, rhs: Tree = EmptyTree) = addDef(position(sym), ValDef(sym, rhs))

      /* Add `newdefs` to `stats`, removing any abstract method definitions
       * in `stats` that are matched by some symbol defined in
       * `newDefs`.
       */
      def add(stats: List[Tree], newDefs: List[Tree]) = {
        val newSyms = newDefs map (_.symbol)
        def isNotDuplicate(tree: Tree) = tree match {
          case DefDef(_, _, _, _, _, _) =>
            val sym = tree.symbol
            !(sym.isDeferred &&
              (newSyms exists (nsym => nsym.name == sym.name && (nsym.tpe matches sym.tpe))))
          case _ =>
            true
        }
        if (newDefs.isEmpty) stats
        else newDefs ::: (stats filter isNotDuplicate)
      }

      /* If `stat` is a superaccessor, complete it by adding a right-hand side.
       * Note: superaccessors are always abstract until this point.
       *  The method to call in a superaccessor is stored in the accessor symbol's alias field.
       * The rhs is:
       *   super.A(xs)  where A is the super accessor's alias and xs are its formal parameters.
       * This rhs is typed and then mixin transformed.
       */
      def completeSuperAccessor(stat: Tree) = stat match {
        case DefDef(_, _, _, vparams :: Nil, _, EmptyTree) if stat.symbol.isSuperAccessor =>
          val body = atPos(stat.pos)(Apply(SuperSelect(clazz, stat.symbol.alias), vparams map (v => Ident(v.symbol))))
          val pt   = stat.symbol.tpe.resultType

          copyDefDef(stat)(rhs = enteringMixin(transform(localTyper.typed(body, pt))))
        case _ =>
          stat
      }

      /*
       *  Return the bitmap field for 'offset'. Depending on the hierarchy it is possible to reuse
       *  the bitmap of its parents. If that does not exist yet we create one.
       */
      def bitmapFor(clazz0: Symbol, offset: Int, field: Symbol): Symbol = {
        val category   = bitmapCategory(field)
        val bitmapName = nme.newBitmapName(category, offset / flagsPerBitmap(field)).toTermName
        val sym        = clazz0.info.decl(bitmapName)

        assert(!sym.isOverloaded, sym)

        def createBitmap: Symbol = {
          val bitmapKind =  bitmapKindForCategory(category)
          val sym = clazz0.newVariable(bitmapName, clazz0.pos) setInfo bitmapKind.tpe
          enteringTyper(sym addAnnotation VolatileAttr)

          category match {
            case nme.BITMAP_TRANSIENT | nme.BITMAP_CHECKINIT_TRANSIENT => sym addAnnotation TransientAttr
            case _                                                     =>
          }
          val init = bitmapKind match {
            case BooleanClass => ValDef(sym, FALSE)
            case _            => ValDef(sym, ZERO)
          }

          sym setFlag PrivateLocal
          clazz0.info.decls.enter(sym)
          addDef(clazz0.pos, init)
          sym
        }

        sym orElse createBitmap
      }

      def maskForOffset(offset: Int, sym: Symbol, kind: ClassSymbol): Tree = {
        def realOffset = offset % flagsPerBitmap(sym)
        if (kind == LongClass ) LIT(1L << realOffset) else LIT(1 << realOffset)
      }

      /* Return an (untyped) tree of the form 'Clazz.this.bmp = Clazz.this.bmp | mask'. */
      def mkSetFlag(clazz: Symbol, offset: Int, valSym: Symbol, kind: ClassSymbol): Tree = {
        val bmp      = bitmapFor(clazz, offset, valSym)
        def mask     = maskForOffset(offset, valSym, kind)
        def x        = This(clazz) DOT bmp
        def newValue = if (kind == BooleanClass) TRUE else (x GEN_| (mask, kind))

        x === newValue
      }

      /* Return an (untyped) tree of the form 'clazz.this.bitmapSym & mask (==|!=) 0', the
       * precise comparison operator depending on the value of 'equalToZero'.
       */
      def mkTest(clazz: Symbol, mask: Tree, bitmapSym: Symbol, equalToZero: Boolean, kind: ClassSymbol): Tree = {
        val bitmapTree  = (This(clazz) DOT bitmapSym)
        def lhs         = bitmapTree GEN_& (mask, kind)
        kind match {
          case BooleanClass =>
            if (equalToZero)  NOT(bitmapTree)
            else              bitmapTree
          case _            =>
            if (equalToZero)  lhs GEN_== (ZERO, kind)
            else              lhs GEN_!= (ZERO, kind)
        }
      }

      def mkSlowPathDef(clazz: Symbol, lzyVal: Symbol, cond: Tree, syncBody: List[Tree],
                        stats: List[Tree], retVal: Tree, attrThis: Tree, args: List[Tree]): Symbol = {
        val defSym = clazz.newMethod(nme.newLazyValSlowComputeName(lzyVal.name.toTermName), lzyVal.pos, PRIVATE)
        val params = defSym newSyntheticValueParams args.map(_.symbol.tpe)
        defSym setInfoAndEnter MethodType(params, lzyVal.tpe.resultType)
        val rhs: Tree = gen.mkSynchronizedCheck(attrThis, cond, syncBody, stats).changeOwner(currentOwner -> defSym)
        val strictSubst = new TreeSymSubstituterWithCopying(args.map(_.symbol), params)
        addDef(position(defSym), DefDef(defSym, strictSubst(BLOCK(rhs, retVal))))
        defSym
      }

      def mkFastPathLazyBody(clazz: Symbol, lzyVal: Symbol, cond: => Tree, syncBody: List[Tree],
                             stats: List[Tree], retVal: Tree): Tree = {
        mkFastPathBody(clazz, lzyVal, cond, syncBody, stats, retVal, gen.mkAttributedThis(clazz), List())
      }

      def mkFastPathBody(clazz: Symbol, lzyVal: Symbol, cond: => Tree, syncBody: List[Tree],
                        stats: List[Tree], retVal: Tree, attrThis: Tree, args: List[Tree]): Tree = {
        val slowPathSym: Symbol = mkSlowPathDef(clazz, lzyVal, cond, syncBody, stats, retVal, attrThis, args)
        If(cond, fn (This(clazz), slowPathSym, args.map(arg => Ident(arg.symbol)): _*), retVal)
      }


      /* Always copy the tree if we are going to perform sym substitution,
       * otherwise we will side-effect on the tree that is used in the fast path
       */
      class TreeSymSubstituterWithCopying(from: List[Symbol], to: List[Symbol]) extends TreeSymSubstituter(from, to) {
        override def transform(tree: Tree): Tree =
          if (tree.hasSymbolField && from.contains(tree.symbol))
            super.transform(tree.duplicate)
          else super.transform(tree.duplicate)

        override def apply[T <: Tree](tree: T): T = if (from.isEmpty) tree else super.apply(tree)
      }

      /*  return a 'lazified' version of rhs. It uses double-checked locking to ensure
       *  initialization is performed at most once. For performance reasons the double-checked
       *  locking is split into two parts, the first (fast) path checks the bitmap without
       *  synchronizing, and if that fails it initializes the lazy val within the
       *  synchronization block (slow path). This way the inliner should optimize
       *  the fast path because the method body is small enough.
       *  Private fields used only in this initializer are subsequently set to null.
       *
       *  @param clazz The class symbol
       *  @param lzyVal The symbol of this lazy field
       *  @param init The tree which initializes the field ( f = <rhs> )
       *  @param offset The offset of this field in the flags bitmap
       *
       *  The result will be a tree of the form
       *  { if ((bitmap&n & MASK) == 0) this.l$compute()
       *    else l$
       *
       *    ...
       *    def l$compute() = { synchronized(this) {
       *      if ((bitmap$n & MASK) == 0) {
       *        init // l$ = <rhs>
       *        bitmap$n = bimap$n | MASK
       *      }}
       *      l$
       *    }
       *
       *    ...
       *    this.f1 = null
       *    ... this.fn = null
       *  }
       *  where bitmap$n is a byte, int or long value acting as a bitmap of initialized values.
       *  The kind of the bitmap determines how many bit indicators for lazy vals are stored in it.
       *  For Int bitmap it is 32 and then 'n' in the above code is: (offset / 32),
       *  the MASK is (1 << (offset % 32)).
       *  If the class contains only a single lazy val then the bitmap is represented
       *  as a Boolean and the condition checking is a simple bool test.
       */
      def mkLazyDef(clazz: Symbol, lzyVal: Symbol, init: List[Tree], retVal: Tree, offset: Int): Tree = {
        def nullify(sym: Symbol) = Select(This(clazz), sym.accessedOrSelf) === LIT(null)

        val bitmapSym = bitmapFor(clazz, offset, lzyVal)
        val kind      = bitmapKind(lzyVal)
        val mask      = maskForOffset(offset, lzyVal, kind)
        def cond      = mkTest(clazz, mask, bitmapSym, equalToZero = true, kind)
        val nulls     = lazyValNullables(lzyVal).toList sortBy (_.id) map nullify
        def syncBody  = init ::: List(mkSetFlag(clazz, offset, lzyVal, kind), UNIT)

        if (nulls.nonEmpty)
          log("nulling fields inside " + lzyVal + ": " + nulls)

        typedPos(init.head.pos)(mkFastPathLazyBody(clazz, lzyVal, cond, syncBody, nulls, retVal))
      }

      def mkCheckedAccessor(clazz: Symbol, retVal: Tree, offset: Int, pos: Position, fieldSym: Symbol): Tree = {
        val sym = fieldSym.getterIn(fieldSym.owner)
        val bitmapSym = bitmapFor(clazz, offset, sym)
        val kind      = bitmapKind(sym)
        val mask      = maskForOffset(offset, sym, kind)
        val msg       = s"Uninitialized field: ${unit.source}: ${pos.line}"
        val result    =
          IF (mkTest(clazz, mask, bitmapSym, equalToZero = false, kind)) .
            THEN (retVal) .
            ELSE (Throw(NewFromConstructor(UninitializedFieldConstructor, LIT(msg))))

        typedPos(pos)(BLOCK(result, retVal))
      }

      /* Complete lazy field accessors. Applies only to classes,
       * for its own (non inherited) lazy fields. If 'checkinit'
       * is enabled, getters that check for the initialized bit are
       * generated, and the class constructor is changed to set the
       * initialized bits.
       */
      def addCheckedGetters(clazz: Symbol, stats: List[Tree]): List[Tree] = {
        def dd(stat: DefDef) = {
          val sym     = stat.symbol
          def isUnit  = sym.tpe.resultType.typeSymbol == UnitClass
          def isEmpty = stat.rhs == EmptyTree

          if (!clazz.isTrait && sym.isLazy && !isEmpty) {
            assert(fieldOffset contains sym, sym)
            deriveDefDef(stat) {
              case t if isUnit => mkLazyDef(clazz, sym, List(t), UNIT, fieldOffset(sym))

              case Block(stats, res) =>
                mkLazyDef(clazz, sym, stats, Select(This(clazz), res.symbol), fieldOffset(sym))

              case t => t // pass specialized lazy vals through
            }
          }
          else if (needsInitFlag(sym) && !isEmpty && !clazz.hasFlag(TRAIT)) {
            assert(fieldOffset contains sym, sym)
            deriveDefDef(stat)(rhs =>
              (mkCheckedAccessor(clazz, _: Tree, fieldOffset(sym), stat.pos, sym))(
                if (sym.tpe.resultType.typeSymbol == UnitClass) UNIT
                else rhs
              )
            )
          }
          else if (sym.isConstructor) {
            deriveDefDef(stat)(addInitBits(clazz, _))
          }
          else if (settings.checkInit && !clazz.isTrait && sym.isSetter) {
            val getter = sym.getterIn(clazz)
            if (needsInitFlag(getter) && fieldOffset.isDefinedAt(getter))
              deriveDefDef(stat)(rhs => Block(List(rhs, localTyper.typed(mkSetFlag(clazz, fieldOffset(getter), getter, bitmapKind(getter)))), UNIT))
            else stat
          }
          else stat
        }
        stats map {
          case defn: DefDef => dd(defn)
          case stat         => stat
        }
      }

      class AddInitBitsTransformer(clazz: Symbol) extends Transformer {
        private def checkedGetter(lhs: Tree) = {
          val sym = clazz.info decl lhs.symbol.getterName suchThat (_.isGetter)
          if (needsInitAndHasOffset(sym)) {
            debuglog("adding checked getter for: " + sym + " " + lhs.symbol.flagString)
            List(localTyper typed mkSetFlag(clazz, fieldOffset(sym), sym, bitmapKind(sym)))
          }
          else Nil
        }
        override def transformStats(stats: List[Tree], exprOwner: Symbol) = {
          // !!! Ident(self) is never referenced, is it supposed to be confirming
          // that self is anything in particular?
          super.transformStats(
            stats flatMap {
              case stat @ Assign(lhs @ Select(This(_), _), rhs) => stat :: checkedGetter(lhs)
              // remove initialization for default values
              case Apply(lhs @ Select(Ident(self), _), EmptyTree.asList) if lhs.symbol.isSetter => Nil
              case stat => List(stat)
            },
            exprOwner
          )
        }
      }

      /* Adds statements to set the 'init' bit for each field initialized
       * in the body of a constructor.
       */
      def addInitBits(clazz: Symbol, rhs: Tree): Tree =
        new AddInitBitsTransformer(clazz) transform rhs

      // begin addNewDefs

      /* Fill the map from fields to offset numbers.
       * Instead of field symbols, the map keeps their getter symbols. This makes
       * code generation easier later.
       */
      def buildBitmapOffsets() {
        def fold(fields: List[Symbol], category: Name) = {
          var idx = 0
          fields foreach { f =>
            fieldOffset(f) = idx
            idx += 1
          }

          if (idx == 0) ()
          else if (idx == 1) bitmapKindForCategory(category) = BooleanClass
          else if (idx < 9)  bitmapKindForCategory(category) = ByteClass
          else if (idx < 33) bitmapKindForCategory(category) = IntClass
          else bitmapKindForCategory(category)               = LongClass
        }
        clazz.info.decls.toList groupBy bitmapCategory foreach {
          case (nme.NO_NAME, _)            => ()
          case (category, fields)          => fold(fields, category)
        }
      }
      buildBitmapOffsets()
      var stats1 = addCheckedGetters(clazz, stats)

      def getterBody(getter: Symbol) = {
        assert(getter.isGetter)
        val readValue = getter.tpe match {
          // A field "final val f = const" in a trait generates a getter with a ConstantType.
          case MethodType(Nil, ConstantType(c)) =>
            Literal(c)
          case _ =>
            // if it is a mixed-in lazy value, complete the accessor
            if (getter.isLazy) {
              val isUnit    = isUnitGetter(getter)
              val initCall  = Apply(SuperSelect(clazz, initializer(getter)), Nil)
              val selection = fieldAccess(getter)
              val init      = if (isUnit) initCall else atPos(getter.pos)(Assign(selection, initCall))
              val returns   = if (isUnit) UNIT else selection
              mkLazyDef(clazz, getter, List(init), returns, fieldOffset(getter))
            }
            // For a field of type Unit in a trait, no actual field is generated when being mixed in.
            else if (isUnitGetter(getter)) UNIT
            else fieldAccess(getter)
        }
        if (!needsInitFlag(getter)) readValue
        else mkCheckedAccessor(clazz, readValue, fieldOffset(getter), getter.pos, getter)
      }

      def setterBody(setter: Symbol) = {
        val getter = setter.getterIn(clazz)

        // A trait with a field of type Unit creates a trait setter (invoked by the
        // implementation class constructor), like for any other trait field.
        // However, no actual field is created in the class that mixes in the trait.
        // Therefore the setter does nothing (except setting the -Xcheckinit flag).

        val setInitFlag =
          if (!needsInitFlag(getter)) Nil
          else List(mkSetFlag(clazz, fieldOffset(getter), getter, bitmapKind(getter)))

        val fieldInitializer =
          if (isUnitGetter(getter)) Nil
          else List(Assign(fieldAccess(setter), Ident(setter.firstParam)))

        (fieldInitializer ::: setInitFlag) match {
          case Nil => UNIT
          // If there's only one statement, the Block factory does not actually create a Block.
          case stats => Block(stats: _*)
        }
      }

      def isUnitGetter(getter: Symbol) = getter.tpe.resultType.typeSymbol == UnitClass
      def fieldAccess(accessor: Symbol) = Select(This(clazz), accessor.accessed)

      def isOverriddenSetter(sym: Symbol) =
        nme.isTraitSetterName(sym.name) && {
          val other = sym.nextOverriddenSymbol
          isOverriddenAccessor(other.getterIn(other.owner), clazz.info.baseClasses)
        }

      // for all symbols `sym` in the class definition, which are mixed in:
      for (sym <- clazz.info.decls ; if sym hasFlag MIXEDIN) {
        // if current class is a trait, add an abstract method for accessor `sym`
        if (clazz.isTrait) {
          addDefDef(sym)
        } else {
          // if class is not a trait add accessor definitions
          if (isConcreteAccessor(sym)) {
            // add accessor definitions
            addDefDef(sym, {
              if (sym.isSetter) {
                // If this is a setter of a mixed-in field which is overridden by another mixin,
                // the trait setter of the overridden one does not need to do anything - the
                // trait setter of the overriding field will initialize the field.
                if (isOverriddenSetter(sym)) UNIT
                else setterBody(sym)
              }
              else getterBody(sym)
            })
          }
          else if (sym.isModule && !(sym hasFlag LIFTED | BRIDGE)) {
            // Moved to Refchecks
          }
          else if (!sym.isMethod) {
            // add fields
            addValDef(sym)
          }
          else if (sym.isSuperAccessor) {
            // add superaccessors
            addDefDef(sym)
          }
          else {
            // add forwarders
            assert(sym.alias != NoSymbol, (sym, sym.debugFlagString, clazz))
            // debuglog("New forwarder: " + sym.defString + " => " + sym.alias.defString)
            if (!sym.isMacro) addDefDef(sym, Apply(SuperSelect(clazz, sym.alias), sym.paramss.head.map(Ident(_))))
          }
        }
      }
      stats1 = add(stats1, newDefs.toList)
      if (clazz.isTrait) stats1 =
        stats1.filter {
          case vd: ValDef =>
            // TODO do we get here?
            false
          case _ => true
        }
      if (!clazz.isTrait) stats1 = stats1 map completeSuperAccessor
      stats1
    }

    private def nullableFields(templ: Template): Map[Symbol, Set[Symbol]] = {
      val scope = templ.symbol.owner.info.decls
      // if there are no lazy fields, take the fast path and save a traversal of the whole AST
      if (scope exists (_.isLazy)) {
        val map = mutable.Map[Symbol, Set[Symbol]]() withDefaultValue Set()
        // check what fields can be nulled for
        for ((field, users) <- singleUseFields(templ); lazyFld <- users if !lazyFld.accessed.hasAnnotation(TransientAttr))
          map(lazyFld) += field

        map.toMap
      }
      else Map()
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
     *    - change `this` in implementation modules to references to the self parameter
     *    - refer to fields in some implementation class via an abstract method in the interface.
     */
    private def postTransform(tree: Tree): Tree = {
      val sym = tree.symbol

      tree match {
        case templ @ Template(parents, self, body) =>
          // change parents of templates to conform to parents in the symbol info
          val parents1 = currentOwner.info.parents map (t => TypeTree(t) setPos tree.pos)
          // mark fields which can be nulled afterward
          lazyValNullables = nullableFields(templ) withDefaultValue Set()
          // Remove bodies of accessors in traits - TODO: after PR #5141 (fields refactoring), this might be a no-op
          val bodyEmptyAccessors = if (!sym.enclClass.isTrait) body else body mapConserve {
            case dd: DefDef if dd.symbol.isAccessor && !dd.symbol.isLazy =>
              deriveDefDef(dd)(_ => EmptyTree)
            case tree => tree
          }
          // add all new definitions to current class or interface
          val body1 = addNewDefs(currentOwner, bodyEmptyAccessors)
          body1 foreach {
            case dd: DefDef if isTraitMethodRequiringStaticImpl(dd) =>
              dd.symbol.updateAttachment(NeedStaticImpl)
            case _ =>
          }
          treeCopy.Template(tree, parents1, self, body1)

        case Select(qual, name) if sym.owner.isTrait && !sym.isMethod =>
          // refer to fields in some trait an abstract getter in the interface.
          val ifaceGetter = sym getterIn sym.owner

          if (ifaceGetter == NoSymbol) abort("No getter for " + sym + " in " + sym.owner)
          else typedPos(tree.pos)((qual DOT ifaceGetter)())

        case Assign(Apply(lhs @ Select(qual, _), List()), rhs) =>
          // assign to fields in some trait via an abstract setter in the interface.
          // Note that the case above has added the empty application.
          val setter = lhs.symbol.setterIn(lhs.symbol.owner.tpe.typeSymbol) setPos lhs.pos

          typedPos(tree.pos)((qual DOT setter)(rhs))

        case _ =>
          tree
      }
    }

    /** The main transform method.
     *  This performs pre-order traversal preTransform at mixin phase;
     *  when coming back, it performs a postTransform at phase after.
     */
    override def transform(tree: Tree): Tree = {
      val saved = localTyper
      val tree1 = super.transform(preTransform(tree))
      // localTyper needed when not flattening inner classes. parts after an
      // inner class will otherwise be typechecked with a wrong scope
      try exitingMixin(postTransform(tree1))
      finally localTyper = saved
    }
  }

  private def isTraitMethodRequiringStaticImpl(dd: DefDef): Boolean = {
    val sym = dd.symbol
    dd.rhs.nonEmpty &&
      sym.owner.isTrait &&
      !sym.isPrivate && // no need to put implementations of private methods into a static method
      !sym.hasFlag(Flags.STATIC)
  }

  case object NeedStaticImpl extends PlainAttachment
}
