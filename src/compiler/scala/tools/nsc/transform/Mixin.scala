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

// TODO: move lazy vals bitmap creation to lazy vals phase now that lazy vals are mixed in during fields
trait InitBitmaps extends Transform with ast.TreeDSL {
  import global._
  import definitions._
  import CODE._

  /** Map a field symbol to a unique integer denoting its position in the class layout.
    * For each class, fields defined by the class come after inherited fields.
    * Mixed-in fields count as fields defined by the class itself.
    */
  val fieldOffset = perRunCaches.newMap[Symbol, Int]()

  val bitmapKindForCategory = perRunCaches.newMap[Name, ClassSymbol]()

  class InitializationTransformer extends Transformer {
    /** The typer */
    protected var localTyper: erasure.Typer = _
    protected def typedPos(pos: Position)(tree: Tree): Tree = localTyper.typedPos(pos)(tree)

    trait AccessorInitialization {
      protected val clazz: Symbol
      protected val templ: Template

      def addDefDef(sym: Symbol, rhs: Tree = EmptyTree): Unit
      def addValDef(sym: Symbol, rhs: Tree = EmptyTree): Unit

      type WithInitChecks
      def addInitChecks(stats: List[Tree]): WithInitChecks
      def implementWithNewDefs(stats: WithInitChecks): List[Tree]

      def accessorBody(sym: Symbol) =
        if (sym.isSetter) setterBody(sym, sym.getterIn(clazz)) else getterBody(sym)

      protected def getterBody(getter: Symbol): Tree = {
        assert(getter.isGetter)
        assert(getter.hasFlag(PARAMACCESSOR))

        fieldAccess(getter)
      }

      protected def setterBody(setter: Symbol, getter: Symbol): Tree = {
        assert(getter.hasFlag(PARAMACCESSOR), s"missing implementation for non-paramaccessor $setter in $clazz")

        Assign(fieldAccess(setter), Ident(setter.firstParam))
      }

      private def fieldAccess(accessor: Symbol) =
        Select(This(clazz), accessor.accessed)

      def completeSuperAccessor(stat: Tree): Tree
    }

  }
}


abstract class Mixin extends InfoTransform with ast.TreeDSL with InitBitmaps {
  import global._
  import definitions._
  import CODE._


  /** The name of the phase: */
  val phaseName: String = "mixin"

  /** Some trait methods need to be implemented in subclasses, so they cannot be private.
    *
    * We used to publicize during explicitouter (for some reason), so the condition is a bit more involved now it's done here
    * (need to exclude lambdaLIFTED methods, as they do no exist during explicitouter and thus did not need to be excluded...)
    *
    * They may be protected, now that traits are compiled 1:1 to interfaces.
    * The same disclaimers about mapping Scala's notion of visibility to Java's apply:
    * we cannot emit PROTECTED methods in interfaces on the JVM,
    * but knowing that these trait methods are protected means we won't emit static forwarders.
    *
    * JVMLS: "Methods of interfaces may have any of the flags in Table 4.6-A set
    * except ACC_PROTECTED, ACC_FINAL, ACC_SYNCHRONIZED, and ACC_NATIVE (JLS §9.4)."
    *
    * TODO: can we just set the right flags from the start??
    *  could we use the final flag to indicate a private method is really-really-private?
    */
  def publicizeTraitMethod(sym: Symbol): Unit = {
    if ((sym hasFlag PRIVATE) && !(sym hasFlag LIFTED) && ( // lambdalifted methods can remain private
        // super accessors by definition must be implemented in a subclass, so can't be private
        // TODO: why are they ever private in a trait to begin with!?!? (could just name mangle them to begin with)
        // TODO: can we add the SYNTHESIZE_IMPL_IN_SUBCLASS flag to super accessors symbols?
        (sym hasFlag SUPERACCESSOR)
        // an accessor / module *may* need to be implemented in a subclass, and thus cannot be private
        // TODO: document how we get here (lambdalift? fields has already made accessors not-private)
        || (sym hasFlag ACCESSOR | MODULE) && (sym hasFlag SYNTHESIZE_IMPL_IN_SUBCLASS)))
      sym.makeNotPrivate(sym.owner)

    // no need to make trait methods not-protected
    // (we used to have to move them to another class when interfaces could not have concrete methods)
    // see note in `synthFieldsAndAccessors` in Fields.scala
    // if (sym hasFlag PROTECTED) sym setFlag notPROTECTED
  }

  /** This map contains a binding (class -> info) if
   *  the class with this info at phase mixinPhase has been treated for mixin composition
   */
  private val treatedClassInfos = perRunCaches.newMap[Symbol, Type]() withDefaultValue NoType


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
    (sym.isMethod || ((sym hasFlag MODULE) && !sym.isStatic))
    && notDeferred(sym)
    && sym.owner.isTrait
    && (!sym.isModule || sym.hasFlag(PRIVATE | LIFTED))
    && (!(sym hasFlag (ACCESSOR | SUPERACCESSOR)) || sym.isLazy)
    && !sym.isPrivate
    && !sym.hasAllFlags(LIFTED | MODULE | METHOD)
    && !sym.isConstructor
    && (!sym.hasFlag(notPRIVATE | LIFTED) || sym.hasFlag(ACCESSOR | SUPERACCESSOR | MODULE))
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

  @inline final def notDeferred(sym: Symbol) = fields.notDeferredOrSynthImpl(sym)

  /** Is member overridden (either directly or via a bridge) in base class sequence `bcs`? */
  def isOverriddenAccessor(member: Symbol, bcs: List[Symbol]): Boolean = beforeOwnPhase {
    def hasOverridingAccessor(clazz: Symbol) = {
      clazz.info.nonPrivateDecl(member.name).alternatives.exists(
        sym =>
          sym.hasFlag(ACCESSOR) &&
          !sym.hasFlag(MIXEDIN) &&
          notDeferred(sym) &&
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

  def publicizeTraitMethods(clazz: Symbol) {
    if (treatedClassInfos(clazz) != clazz.info) {
      treatedClassInfos(clazz) = clazz.info
      assert(phase == currentRun.mixinPhase, phase)

      for (member <- clazz.info.decls) {
        if (member.isMethod) publicizeTraitMethod(member)
        else {
          assert(member.isTerm && !member.isDeferred, member)
          // disable assert to support compiling against code compiled by an older compiler (until we re-starr)
          // assert(member hasFlag LAZY | PRESUPER, s"unexpected $member in $clazz ${member.debugFlagString}")
          // lazy vals still leave field symbols lying around in traits -- TODO: never emit them to begin with
          // ditto for early init vals
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
        resetFlag DEFERRED
    )

    /* Mix in members of implementation class mixinClass into class clazz */
    def mixinTraitForwarders(mixinClass: Symbol) {
      for (member <- mixinClass.info.decls ; if isImplementedStatically(member)) {
        member overridingSymbol clazz match {
          case NoSymbol =>
            val isMemberOfClazz = clazz.info.findMember(member.name, 0, 0L, stableOnly = false).alternatives.contains(member)
            if (isMemberOfClazz) {
              def genForwarder(): Unit = {
                cloneAndAddMixinMember(mixinClass, member).asInstanceOf[TermSymbol] setAlias member
              }

              if (settings.XgenMixinForwarders) genForwarder()
              else {

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
                  genForwarder()
                else if (!settings.nowarnDefaultJunitMethods && JUnitTestClass.exists && member.hasAnnotation(JUnitTestClass))
                  warning(member.pos, "JUnit tests in traits that are compiled as default methods are not executed by JUnit 4. JUnit 5 will fix this issue.")
              }
            }

          case _        =>
        }
      }
    }

    /* Mix in members of trait mixinClass into class clazz.
     */
    def mixinTraitMembers(mixinClass: Symbol) {
      // For all members of a trait's interface do:
      for (mixinMember <- mixinClass.info.decls) {
        if (mixinMember.hasFlag(SUPERACCESSOR)) { // mixin super accessors
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
        else if (mixinMember.hasFlag(ACCESSOR) && notDeferred(mixinMember)
                 && (mixinMember hasFlag PARAMACCESSOR)
                 && !isOverriddenAccessor(mixinMember, clazz.info.baseClasses)) {
          // mixin accessor for constructor parameter
          // (note that a paramaccessor cannot have a constant type as it must have a user-defined type)
          cloneAndAddMixinMember(mixinClass, mixinMember)

          val name = mixinMember.name

          if (!nme.isSetterName(name)) {
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
              (PrivateLocal)
              | (mixinMember getFlag MUTABLE)
              | (if (mixinMember.hasStableFlag) 0 else MUTABLE)
              )

            addMember(clazz, sym setFlag newFlags setAnnotations accessed.annotations)
          }
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
      publicizeTraitMethods(mc)
      mixinTraitMembers(mc)
      mixinTraitForwarders(mc)
    }
  }

  override def transformInfo(sym: Symbol, tp: Type): Type = tp

// --------- term transformation -----------------------------------------------

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new MixinTransformer(unit)

  class MixinTransformer(unit : CompilationUnit) extends InitializationTransformer {

    /** The rootContext used for typing */
    private val rootContext =
      erasure.NoContext.make(EmptyTree, rootMirror.RootClass, newScope)


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
            publicizeTraitMethods(currentOwner)

          tree

        case _ => tree
      }
    }

    def accessorInitialization(clazz: Symbol, templ: Template): AccessorInitialization =
      if (settings.checkInit) new AddsCheckInitDefs(clazz, templ)
      else new StandardAccessorInitialization(clazz, templ)


    protected class StandardAccessorInitialization(val clazz: Symbol, val templ: Template) extends AccessorInitialization {
      /** Map lazy values to the fields they should null after initialization. */
      private lazy val lazyValNullables: Map[Symbol, List[Symbol]] =
      // if there are no lazy fields, take the fast path and save a traversal of the whole AST
        if (!clazz.info.decls.exists(_.isLazy)) Map()
        else {
          // A map of single-use fields to the lazy value that uses them during initialization.
          // Each field has to be private and defined in the enclosing class, and there must
          // be exactly one lazy value using it.
          //
          // Such fields will be nulled after the initializer has memoized the lazy value.
          val singleUseFields: Map[Symbol, List[Symbol]] = {
            val usedIn = mutable.HashMap[Symbol, List[Symbol]]() withDefaultValue Nil

            object SingleUseTraverser extends Traverser {
              override def traverse(tree: Tree) {
                tree match {
                  case Assign(lhs, rhs) => traverse(rhs) // assignments don't count
                  case _                =>
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
            } toMap
          }

          val map = mutable.Map[Symbol, Set[Symbol]]() withDefaultValue Set()
          // check what fields can be nulled for
          for ((field, users) <- singleUseFields; lazyFld <- users if !lazyFld.accessed.hasAnnotation(TransientAttr))
            map(lazyFld) += field

          map.mapValues(_.toList sortBy (_.id)).toMap
        }

      // overridden in AddsCheckInitDefs
      protected def addCheckedInitCheck(stat: DefDef): DefDef = stat

      // ByteClass, IntClass, LongClass
      protected def bitmapKind(field: Symbol): ClassSymbol = bitmapKindForCategory(bitmapCategory(field))

      /** Examines the symbol and returns a name indicating what brand of
        * bitmap it requires.  The possibilities are the BITMAP_* vals
        * defined in StdNames.  If it needs no bitmap, nme.NO_NAME.
        *
        * bitmaps for checkinit fields are not inherited
        */
      protected def bitmapCategory(field: Symbol): Name = {
        import nme._

        def isFieldWithBitmap(field: Symbol) = {
          field.info // ensure that nested objects are transformed
          // For checkinit consider normal value getters
          // but for lazy values only take into account lazy getters
          field.isLazy && field.isMethod && !field.isDeferred
        }

        if (isFieldWithBitmap(field))
          if (isTransientField(field)) BITMAP_TRANSIENT else BITMAP_NORMAL
        else if (needsInitFlag(field) && !field.isDeferred)
          if (isTransientField(field)) BITMAP_CHECKINIT_TRANSIENT else BITMAP_CHECKINIT
        else NO_NAME
      }

      private val _newDefs = mutable.ListBuffer[Tree]()

      /** Attribute given tree and anchor at given position */
      private def attributedDef(pos: Position, tree: Tree): Tree = {
        debuglog("add new def to " + clazz + ": " + tree)
        typedPos(pos)(tree)
      }


      /** The position of given symbol, or, if this is undefined,
        * the position of the current class.
        */
      private def position(sym: Symbol) =
        if (sym.pos == NoPosition) clazz.pos else sym.pos

      /** Add tree at given position as new definition */
      private def addDef(pos: Position, tree: ValOrDefDef): Unit =
        _newDefs += attributedDef(pos, tree)

      /** Add new method definition.
        *
        * @param sym The method symbol.
        * @param rhs The method body.
        */
      def addDefDef(sym: Symbol, rhs: Tree = EmptyTree) = addDef(position(sym), DefDef(sym, rhs))
      def addValDef(sym: Symbol, rhs: Tree = EmptyTree) = addDef(position(sym), ValDef(sym, rhs))



      protected def isTransientField(field: Symbol) = field.accessedOrSelf hasAnnotation TransientAttr

      // overridden in AddsCheckInitDefs
      protected def needsInitFlag(sym: Symbol): Boolean = false

      protected def isUnitGetter(sym: Symbol) = sym.tpe.resultType.typeSymbol == UnitClass

      private def flagsPerBitmap(field: Symbol): Int = bitmapKind(field) match {
        case BooleanClass => 1
        case ByteClass    => 8
        case IntClass     => 32
        case LongClass    => 64
      }


      type WithInitChecks = List[Tree]

      /** Complete `stats` with init checks and bitmaps,
        * removing any abstract method definitions in `stats` that are
        * matched by some symbol defined by a tree previously passed to `addDef`.
        */
      def implementWithNewDefs(stats: WithInitChecks): List[Tree] = {
        val newDefs = _newDefs.toList
        val newSyms = newDefs map (_.symbol)
        def isNotDuplicate(tree: Tree) = tree match {
          case DefDef(_, _, _, _, _, _) =>
            val sym = tree.symbol
            !(sym.isDeferred &&
              (newSyms exists (nsym => nsym.name == sym.name && (nsym.tpe matches sym.tpe))))
          case _ => true
        }
        if (newDefs.isEmpty) stats
        else newDefs ::: (stats filter isNotDuplicate)
      }

      /** If `stat` is a superaccessor, complete it by adding a right-hand side.
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
      def bitmapFor(offset: RunId, field: global.Symbol): Symbol = {
        val category = bitmapCategory(field)
        val bitmapName = nme.newBitmapName(category, offset / flagsPerBitmap(field)).toTermName
        val sym = clazz.info.decl(bitmapName)

        assert(!sym.isOverloaded, sym)

        def createBitmap: Symbol = {
          val bitmapKind = bitmapKindForCategory(category)
          val sym = clazz.newVariable(bitmapName, clazz.pos) setInfo bitmapKind.tpe
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
          clazz.info.decls.enter(sym)
          addDef(clazz.pos, init)
          sym
        }

        sym orElse createBitmap
      }

      protected def maskForOffset(offset: Int, sym: Symbol, kind: ClassSymbol): Tree = {
        def realOffset = offset % flagsPerBitmap(sym)
        if (kind == LongClass) LIT(1L << realOffset) else LIT(1 << realOffset)
      }

      /* Return an (untyped) tree of the form 'Clazz.this.bmp = Clazz.this.bmp | mask'. */
      def mkSetFlag(offset: Int, valSym: Symbol, kind: ClassSymbol): Tree = {
        val bmp = bitmapFor(offset, valSym)
        def mask = maskForOffset(offset, valSym, kind)
        def x = This(clazz) DOT bmp
        def newValue = if (kind == BooleanClass) TRUE else (x GEN_|(mask, kind))

        x === newValue
      }

      /** Return an (untyped) tree of the form 'clazz.this.bitmapSym & mask (==|!=) 0', the
        * precise comparison operator depending on the value of 'equalToZero'.
        */
      protected def mkTest(mask: Tree, bitmapSym: Symbol, equalToZero: Boolean, kind: ClassSymbol): Tree = {
        val bitmapTree = (This(clazz) DOT bitmapSym)
        def lhs = bitmapTree GEN_&(mask, kind)
        kind match {
          case BooleanClass =>
            if (equalToZero) NOT(bitmapTree)
            else bitmapTree
          case _            =>
            if (equalToZero) lhs GEN_==(ZERO, kind)
            else lhs GEN_!=(ZERO, kind)
        }
      }


      /** return a 'lazified' version of rhs. It uses double-checked locking to ensure
        * initialization is performed at most once. For performance reasons the double-checked
        * locking is split into two parts, the first (fast) path checks the bitmap without
        * synchronizing, and if that fails it initializes the lazy val within the
        * synchronization block (slow path). This way the inliner should optimize
        * the fast path because the method body is small enough.
        * Private fields used only in this initializer are subsequently set to null.
        *
        * @param lzyVal The symbol of this lazy field
        * @param init   The tree which initializes the field ( f = <rhs> )
        * @param offset The offset of this field in the flags bitmap
        *
        *               The result will be a tree of the form
        *               { if ((bitmap&n & MASK) == 0) this.l$compute()
        *               else l$
        *
        *               ...
        *               def l$compute() = { synchronized(this) {
        *               if ((bitmap$n & MASK) == 0) {
        *               init // l$ = <rhs>
        *               bitmap$n = bimap$n | MASK
        *               }}
        *               l$
        *               }
        *
        *               ...
        *    this.f1 = null
        *               ... this.fn = null
        *               }
        *               where bitmap$n is a byte, int or long value acting as a bitmap of initialized values.
        *               The kind of the bitmap determines how many bit indicators for lazy vals are stored in it.
        *               For Int bitmap it is 32 and then 'n' in the above code is: (offset / 32),
        *               the MASK is (1 << (offset % 32)).
        *               If the class contains only a single lazy val then the bitmap is represented
        *               as a Boolean and the condition checking is a simple bool test.
        */
      private def mkLazyMemberDef(lzyVal: Symbol, init: List[Tree], retVal: Tree, offset: Int): Tree = {
        def nullify(sym: Symbol) = Select(This(clazz), sym.accessedOrSelf) === LIT(null)

        val bitmapSym = bitmapFor(offset, lzyVal)
        val kind = bitmapKind(lzyVal)
        val mask = maskForOffset(offset, lzyVal, kind)
        val nulls = lazyValNullables.getOrElse(lzyVal, Nil) map nullify

        if (nulls.nonEmpty)
          log("nulling fields inside " + lzyVal + ": " + nulls)

        val pos = if (lzyVal.pos != NoPosition) lzyVal.pos else clazz.pos // TODO: is the else branch ever taken?
        val slowPathSym =
          clazz.newMethod(nme.newLazyValSlowComputeName(lzyVal.name.toTermName), pos, PRIVATE) setInfoAndEnter MethodType(Nil, lzyVal.tpe.resultType)

        def thisRef = gen.mkAttributedThis(clazz)
        def cond = mkTest(mask, bitmapSym, equalToZero = true, kind)

        val statsToSynch = init ::: List(mkSetFlag(offset, lzyVal, kind), UNIT)
        val synchedRhs = gen.mkSynchronizedCheck(thisRef, cond, statsToSynch, nulls)
        addDef(pos, DefDef(slowPathSym, Block(List(synchedRhs.changeOwner(currentOwner -> slowPathSym)), retVal)))

        typedPos(init.head.pos)(If(cond, Apply(Select(thisRef, slowPathSym), Nil), retVal))
      }


      def addInitChecks(stats: List[Tree]): WithInitChecks = {
        /* Fill the map from fields to offset numbers.
         * Instead of field symbols, the map keeps their getter symbols. This makes
         * code generation easier later.
         */
        def buildBitmapOffsets: Unit = {
          def fold(fields: List[Symbol], category: Name) = {
            var idx = 0
            fields foreach { f =>
              fieldOffset(f) = idx
              idx += 1
            }

            if (idx == 0) ()
            else if (idx == 1) bitmapKindForCategory(category) = BooleanClass
            else if (idx < 9) bitmapKindForCategory(category) = ByteClass
            else if (idx < 33) bitmapKindForCategory(category) = IntClass
            else bitmapKindForCategory(category) = LongClass
          }
          clazz.info.decls.toList groupBy bitmapCategory foreach {
            case (nme.NO_NAME, _)   => ()
            case (category, fields) => fold(fields, category)
          }
        }

        /** Complete lazy field accessors. Applies only to classes,
          * for its own (non inherited) lazy fields. If 'checkinit'
          * is enabled, getters that check for the initialized bit are
          * generated, and the class constructor is changed to set the
          * initialized bits.
          */
        def addInitCheck(stat: DefDef): DefDef = {
          val sym = stat.symbol

          if (!clazz.isTrait && sym.isLazy && stat.rhs != EmptyTree) {
            assert(fieldOffset contains sym, sym)
            deriveDefDef(stat) {
              case t if isUnitGetter(sym) =>
                mkLazyMemberDef(sym, List(t), UNIT, fieldOffset(sym))

              case Block(stats, res) =>
                mkLazyMemberDef(sym, stats, Select(This(clazz), res.symbol), fieldOffset(sym))

              case t => t // pass specialized lazy vals through
            }
          }
          else addCheckedInitCheck(stat)
        }

        buildBitmapOffsets

        stats mapConserve {
          case dd: DefDef => addInitCheck(dd)
          case stat       => stat
        }
      }

    }

    protected class AddsCheckInitDefs(clazz: Symbol, templ: Template) extends StandardAccessorInitialization(clazz, templ) {
      /** Does this field require an initialized bit?
        * Note: fields of classes inheriting DelayedInit are not checked.
        * This is because they are neither initialized in the constructor
        * nor do they have a setter (not if they are vals anyway). The usual
        * logic for setting bitmaps does therefore not work for such fields.
        * That's why they are excluded.
        * Note: The `checkinit` option does not check if transient fields are initialized.
        */
      override def needsInitFlag(sym: Symbol): Boolean = (
        sym.isGetter
        && !sym.isInitializedToDefault
        && !isConstantType(sym.info.finalResultType) // SI-4742
        && !sym.hasFlag(PARAMACCESSOR | SPECIALIZED | LAZY)
        && !sym.accessed.hasFlag(PRESUPER)
        && !sym.isOuterAccessor
        && !(sym.owner isSubClass DelayedInitClass)
        && !(sym.accessed hasAnnotation TransientAttr)
        )

      object addInitBitsTransformer extends Transformer {
        private def checkedGetter(lhs: Tree) = {
          val sym = clazz.info decl lhs.symbol.getterName suchThat (_.isGetter)
          if (needsInitFlag(sym) && (fieldOffset contains sym)) {
            debuglog("adding checked getter for: " + sym + " " + lhs.symbol.flagString)
            List(localTyper typed mkSetFlag(fieldOffset(sym), sym, bitmapKind(sym)))
          }
          else Nil
        }
        override def transformStats(stats: List[Tree], exprOwner: Symbol) = {
          // !!! Ident(self) is never referenced, is it supposed to be confirming
          // that self is anything in particular?
          super.transformStats(
            stats flatMap {
              case stat@Assign(lhs@Select(This(_), _), rhs) => stat :: checkedGetter(lhs)
              // remove initialization for default values -- TODO is this case ever hit? constructors does not generate Assigns with EmptyTree for the rhs AFAICT
              case Apply(lhs@Select(Ident(self), _), EmptyTree.asList) if lhs.symbol.isSetter => Nil
              case stat                                                                       => List(stat)
            },
            exprOwner
          )
        }
      }

      /** Adds statements to set the 'init' bit for each field initialized
        * in the body of a constructor.
        * @pre settings.checkInit
        */
      def addInitBits(rhs: Tree): Tree = addInitBitsTransformer transform rhs

      override def addCheckedInitCheck(stat: DefDef): DefDef = {
        val sym = stat.symbol
        if (needsInitFlag(sym) && stat.rhs != EmptyTree && !clazz.hasFlag(TRAIT)) {
          assert(fieldOffset contains sym, sym)
          deriveDefDef(stat)(rhs =>
            (mkCheckedAccessor(_: Tree, fieldOffset(sym), stat.pos, sym)) (
              if (isUnitGetter(sym)) UNIT else rhs
            )
          )
        }
        else if (sym.isConstructor) {
          deriveDefDef(stat)(addInitBits)
        }
        else if (!clazz.isTrait && sym.isSetter) {
          val getter = sym.getterIn(clazz)
          if (needsInitFlag(getter) && fieldOffset.isDefinedAt(getter))
            deriveDefDef(stat)(rhs => Block(List(rhs, localTyper.typed(mkSetFlag(fieldOffset(getter), getter, bitmapKind(getter)))), UNIT))
          else stat
        }
        else stat
      }

      def mkCheckedAccessor(retVal: Tree, offset: Int, pos: Position, fieldSym: Symbol): Tree = {
        val sym = fieldSym.getterIn(fieldSym.owner)
        val bitmapSym = bitmapFor(offset, sym)
        val kind = bitmapKind(sym)
        val mask = maskForOffset(offset, sym, kind)
        val msg = s"Uninitialized field: ${clazz.sourceFile}: ${pos.line}"
        val result =
          IF(mkTest(mask, bitmapSym, equalToZero = false, kind)).
            THEN(retVal).
            ELSE(Throw(NewFromConstructor(UninitializedFieldConstructor, LIT(msg))))

        typedPos(pos)(BLOCK(result, retVal))
      }

      // TODO: need to run this on all accessor bodies (after fields refactoring, this is only run on accessors mixed in during mixins, which is only PRESUPER | PARAMACCESSOR)
      override def getterBody(getter: Symbol) = {
        if (!needsInitFlag(getter)) super.getterBody(getter)
        else mkCheckedAccessor(super.getterBody(getter), fieldOffset(getter), getter.pos, getter)
      }

      override def setterBody(setter: Symbol, getter: Symbol) = {
        if (!needsInitFlag(getter)) super.setterBody(setter, getter)
        else Block(List(super.setterBody(setter, getter)), mkSetFlag(fieldOffset(getter), getter, bitmapKind(getter)))
      }
    }


    /** Add all new definitions to a non-trait class
      *
      * These fall into the following categories:
      *    - for a trait interface:
      *       - abstract accessors for all paramaccessor or early initialized fields
      *    - for a non-trait class:
      *       - field and accessor implementations for each inherited paramaccessor or early initialized field
      *       - A super accessor for every super accessor in a mixin class
      *       - Forwarders for all methods that are implemented statically
      *
      * All superaccessors are completed with right-hand sides (@see completeSuperAccessor)
      *
      * @param clazz The class to which definitions are added
      */
    private def addNewDefs(clazz: Symbol, stats: List[Tree], accessorInit: AccessorInitialization): List[Tree] = {
      import accessorInit._

      val statsWithInitChecks = addInitChecks(stats)

      // for all symbols `sym` in the class definition, which are mixed in by mixinTraitMembers
      for (sym <- clazz.info.decls ; if sym hasFlag MIXEDIN) {
        // if current class is a trait, add an abstract method for accessor `sym`
        // ditto for a super accessor (will get an RHS in completeSuperAccessor)
        if (clazz.isTrait || sym.isSuperAccessor) addDefDef(sym)
        // implement methods mixed in from a supertrait (the symbols were created by mixinTraitMembers)
        else if (sym.hasFlag(ACCESSOR) && !sym.hasFlag(DEFERRED)) {
          assert(sym hasFlag (PARAMACCESSOR), s"mixed in $sym from $clazz is not lazy/param?!?")

          // add accessor definitions
          addDefDef(sym, accessorBody(sym))
        }
        else if (!sym.isMethod) addValDef(sym) // field
        else if (!sym.isMacro) { // forwarder
          assert(sym.alias != NoSymbol, (sym, sym.debugFlagString, clazz))
          // debuglog("New forwarder: " + sym.defString + " => " + sym.alias.defString)
          addDefDef(sym, Apply(SuperSelect(clazz, sym.alias), sym.paramss.head.map(Ident(_))))
        }
      }

      val addedStatsWithInitBitsAndChecks = implementWithNewDefs(statsWithInitChecks)

      if (clazz.isTrait)
        addedStatsWithInitBitsAndChecks filter {
          case vd: ValDef => assert(vd.symbol.hasFlag(PRESUPER | PARAMACCESSOR), s"unexpected valdef $vd in trait $clazz"); false
          case _ => true
        }
      else {
        addedStatsWithInitBitsAndChecks map completeSuperAccessor
      }
    }

    /** The transform that gets applied to a tree after it has been completely
     *  traversed and possible modified by a preTransform.
     *  This step will
     *    - change parents of templates to conform to parents in the symbol info
     *    - add all new definitions to a class or interface
     *    - remove widening casts
     *    - change calls to methods which are defined only in implementation classes
     *      to static calls of methods in implementation modules (@see staticCall)
     *    - change super calls to methods in implementation classes to static calls
     *      (@see staticCall)
     */
    private def postTransform(tree: Tree): Tree = {
      val sym = tree.symbol

      tree match {
        case templ @ Template(parents, self, body) =>
          // change parents of templates to conform to parents in the symbol info
          val parents1 = currentOwner.info.parents map (t => TypeTree(t) setPos tree.pos)

          // mark fields which can be nulled afterward
          val accessorInit = accessorInitialization(currentOwner, templ)

          // add all new definitions to current class or interface
          val statsWithNewDefs = addNewDefs(currentOwner, body, accessorInit)
          statsWithNewDefs foreach {
            case dd: DefDef if isTraitMethodRequiringStaticImpl(dd) =>
              dd.symbol.updateAttachment(NeedStaticImpl)
            case _ =>
          }
          treeCopy.Template(tree, parents1, self, statsWithNewDefs)

        case Select(qual, name) if sym.owner.isTrait && !sym.isMethod =>
          assert(sym.hasFlag(PARAMACCESSOR | PRESUPER), s"!!! Unexpected reference to field $sym in trait $currentOwner")

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
