/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package transform

import symtab._
import Flags._
import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.NameTransformer
import scala.reflect.internal.util.ListOfNil


abstract class Mixin extends Transform with ast.TreeDSL with AccessorSynthesis {
  import global._
  import definitions._
  import CODE._


  /** The name of the phase: */
  val phaseName: String = "mixin"

  /** Some trait methods need to be implemented in subclasses, so they cannot be private.
    *
    * We used to publicize during explicitouter (for some reason), so the condition is a bit more involved now it's done here
    * (need to exclude lambdaLIFTED methods, as they do not exist during explicitouter and thus did not need to be excluded...)
    *
    * They may be protected, now that traits are compiled 1:1 to interfaces.
    * The same disclaimers about mapping Scala's notion of visibility to Java's apply:
    * we cannot emit PROTECTED methods in interfaces on the JVM,
    * but knowing that these trait methods are protected means we won't emit static forwarders.
    *
    * JVMS: "Methods of interfaces may have any of the flags in Table 4.6-A set
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
   *  mixin transform is RHS of the method body (destined to be in an interface default method)
   *
   *  To be statically implemented, a member must be a method that belonged to the trait's implementation class
   *  before (i.e. it is not abstract). Not statically implemented are
   *   - non-private modules: these are implemented directly in the mixin composition class
   *     (private modules, on the other hand, are implemented statically, but their
   *      module variable is not. all such private modules are lifted, because
   *      non-lifted private modules have been eliminated in ExplicitOuter)
   *   - field accessors and superaccessors
   */
  private def isImplementedStatically(sym: Symbol) = (
    (sym.isMethod || ((sym hasFlag MODULE) && !sym.isStatic))
      // TODO:       ^^^ non-static modules should have been turned into methods by fields by now, no? maybe the info transformer hasn't run???
    && notDeferred(sym)
    && sym.owner.isTrait
    && (!sym.isModule || sym.hasFlag(PRIVATE | LIFTED))
    && (!(sym hasFlag (ACCESSOR | SUPERACCESSOR)) || (sym hasFlag LAZY))
    && !sym.isPrivate
    && !sym.hasAllFlags(LIFTED | MODULE | METHOD)
    && !sym.isConstructor
    && (!sym.hasFlag(notPRIVATE | LIFTED) || sym.hasFlag(ACCESSOR | SUPERACCESSOR | MODULE))
  )



  /** Returns the symbol that is accessed by a super-accessor in a mixin composition.
   *
   *  @param base       The class in which everything is mixed together
   *  @param acc        The symbol statically referred to by the superaccessor in the trait
   *  @param mixinClass The mixin class that produced the superaccessor
   */
  private def rebindSuper(base: Symbol, acc: Symbol, mixinClass: Symbol): Symbol =
    exitingSpecialize {
      val site = base.thisType
      // the specialized version T$sp of a trait T will have a super accessor that has the same alias
      // as the super accessor in trait T; we must rebind super
      // from the vantage point of the original trait T, not the specialized T$sp
      // (it's inserted in the base class seq late in the game and doesn't count as a super class in the super-call scheme)
      val superTargetClass = if (mixinClass.isSpecialized) unspecializedSymbol(mixinClass) else mixinClass
      var bcs = base.info.baseClasses.dropWhile(superTargetClass != _).tail
      var sym: Symbol = NoSymbol

      // println(s"starting rebindsuper $base mixing in from $mixinClass: $acc : ${acc.tpe} of ${acc.owner} ; looking for super in $bcs (all bases: ${base.info.baseClasses})")

      // don't rebind to specialized members unless we're looking for the super of a specialized member,
      // since we can't jump back and forth between the unspecialized name and specialized one
      // (So we jump into the non-specialized world and stay there until we hit our super.)
      val likeSpecialized = if (acc.isSpecialized) 0 else SPECIALIZED

      while (sym == NoSymbol && bcs.nonEmpty) {
        sym = acc.matchingSymbol(bcs.head, site).suchThat(sym => !sym.hasFlag(DEFERRED | BRIDGE | likeSpecialized))
        bcs = bcs.tail
      }

      // println(s"rebound $base from $mixinClass to $sym in ${sym.owner} ($bcs)")

      // Having a matching symbol is not enough: its info should also be a subtype
      // of the superaccessor's type, see test/files/run/t11351.scala
      if ((sym ne acc) && sym.exists && !(sym.isErroneous || (site.memberInfo(sym) <:< site.memberInfo(acc))))
        reporter.error(base.pos, s"illegal trait super target found for $acc required by $mixinClass;" +
                                 s"\n found   : ${exitingTyper{sym.defStringSeenAs(site.memberInfo(sym))}} in ${sym.owner};" +
                                 s"\n expected: ${exitingTyper{acc.defStringSeenAs(site.memberInfo(acc))}} in ${acc.owner}")

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
    // This attachment is used to instruct the backend about which methods in traits require
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
      else {
        sym modifyInfo (_ => forwarderInfo)
        avoidTypeParamShadowing(mixinMember, sym)
        sym
      }
    }
    newSym
  }

  // scala/bug#11523 rename method type parameters that shadow enclosing class type parameters in the host class
  // of the mixin forwarder
  private def avoidTypeParamShadowing(mixinMember: Symbol, forwarder: Symbol): Unit = {
    def isForwarderTparam(sym: Symbol) = {
      val owner = sym.owner
      // TODO fix forwarder's info should not refer to tparams of mixinMember, fix cloning in caller!
      //      try forwarderInfo.cloneInfo(sym)
      owner == forwarder || owner == mixinMember
    }

    val symTparams: mutable.Map[Name, Symbol] = mutable.Map.from(forwarder.typeParams.iterator.map(t => (t.name, t)))
    forwarder.info.foreach {
      case TypeRef(_, tparam, _) if tparam.isTypeParameter && !isForwarderTparam(tparam) =>
        symTparams.get(tparam.name).foreach{ symTparam =>
          debuglog(s"Renaming ${symTparam} (owned by ${symTparam.owner}, a mixin forwarder hosted in ${forwarder.enclClass.fullNameString}) to avoid shadowing enclosing type parameter of ${tparam.owner.fullNameString})")
          symTparam.name = symTparam.name.append(NameTransformer.NAME_JOIN_STRING)
          symTparams.remove(tparam.name) // only rename once
        }
      case _ =>
    }
  }

  def publicizeTraitMethods(clazz: Symbol): Unit = {
    if (treatedClassInfos(clazz) != clazz.info) {
      treatedClassInfos(clazz) = clazz.info
      assert(phase == currentRun.mixinPhase, phase)

      for (member <- clazz.info.decls) {
        if (member.isMethod) publicizeTraitMethod(member)
        else {
          assert(member.isTerm && !member.isDeferred, member)
          assert(member hasFlag PRESUPER, s"unexpected $member in $clazz ${member.debugFlagString}")
          clazz.info.decls.unlink(member)
        }

      }
      debuglog("new defs of " + clazz + " = " + clazz.info.decls)
    }
  }

  def registerRequiredDirectInterface(alias: Symbol, clazz: Symbol, msg: Type => String): Unit = {
    val owner = alias.owner
    if (owner.isJavaDefined && owner.isInterface) {
      if (!clazz.parentSymbolsIterator.contains(owner)) {
        val suggestedParent = exitingTyper(clazz.info.baseType(owner))
        reporter.error(clazz.pos, msg(suggestedParent))
      } else
        erasure.requiredDirectInterfaces.getOrElseUpdate(clazz, mutable.Set.empty) += owner
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
  def addMixedinMembers(clazz: Symbol, unit: CompilationUnit): Unit = {
    def cloneAndAddMixinMember(mixinClass: Symbol, mixinMember: Symbol): Symbol = (
      cloneAndAddMember(mixinClass, mixinMember, clazz)
           setPos clazz.pos
        resetFlag DEFERRED
    )

    /* Mix in members of implementation class mixinClass into class clazz */
    def mixinTraitForwarders(mixinClass: Symbol): Unit = {
      def isMemberOfClass(member: Symbol): Boolean =
        clazz.info.findMember(member.name, 0, 0L, stableOnly = false).alternatives.contains(member)
      for (member <- mixinClass.info.decls)
        if (isImplementedStatically(member) && member.overridingSymbol(clazz) == NoSymbol && isMemberOfClass(member)) {
          def genForwarder(required: Boolean): Unit = {
            val owner = member.owner
            val isJavaInterface = owner.isJavaDefined && owner.isInterface
            if (isJavaInterface && !clazz.parentSymbolsIterator.contains(owner)) {
              if (required) {
                val text = s"Unable to implement a mixin forwarder for $member in $clazz unless interface ${owner.name} is directly extended by $clazz."
                reporter.error(clazz.pos, text)
              }
            } else {
              if (isJavaInterface)
                erasure.requiredDirectInterfaces.getOrElseUpdate(clazz, mutable.Set.empty) += owner
              cloneAndAddMixinMember(mixinClass, member).asInstanceOf[TermSymbol] setAlias member
            }
          }

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

          def generateJUnitForwarder: Boolean = {
            settings.mixinForwarderChoices.isAtLeastJunit &&
              member.annotations.nonEmpty &&
              JUnitAnnotations.exists(annot => annot.exists && member.hasAnnotation(annot))
          }

          def generateSerializationForwarder: Boolean = {
            (member.name == nme.readResolve || member.name == nme.writeReplace) && member.info.paramss == ListOfNil
          }

          if (existsCompetingMethod(clazz.baseClasses) || generateJUnitForwarder || generateSerializationForwarder)
            genForwarder(required = true)
          else if (settings.mixinForwarderChoices.isTruthy)
            genForwarder(required = false)
        }
    }

    /* Mix in members of trait mixinClass into class clazz.
     */
    def mixinTraitMembers(mixinClass: Symbol): Unit = {
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
              registerRequiredDirectInterface(alias1, clazz, parent =>
                s"Unable to implement a super accessor required by trait ${mixinClass.name} unless $parent is directly extended by $clazz.")
              if (alias1.isValue && !alias1.isMethod || alias1.isAccessor)
                reporter.error(clazz.pos, s"parent $mixinClass has a super call to method ${mixinMember.alias.fullNameString}, which binds to the value ${alias1.fullNameString}. Super calls can only target methods.")
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
      unit.registerDependency(mc)
      publicizeTraitMethods(mc)
      mixinTraitMembers(mc)
      mixinTraitForwarders(mc)
    }
  }

// --------- term transformation -----------------------------------------------

  protected def newTransformer(unit: CompilationUnit): AstTransformer =
    new MixinTransformer(unit)

  class MixinTransformer(unit : CompilationUnit) extends AstTransformer with AccessorTreeSynthesis {
    /** The typer */
    private var localTyper: erasure.Typer = _
    protected def typedPos(pos: Position)(tree: Tree): Tree = localTyper.typedPos(pos)(tree)

    /** The rootContext used for typing */
    private val rootContext =
      erasure.NoContext.make(EmptyTree, rootMirror.RootClass, newScope)

    private val nullables = mutable.HashMap[Symbol, Map[Symbol, List[Symbol]]]()

    /** The first transform; called in a pre-order traversal at phase mixin
     *  (that is, every node is processed before its children).
     *  What transform does:
     *   - For every non-trait class, add all mixed in members to the class info.
     *  - For every non-trait class, assign null to singly used private fields after use in lazy initialization.
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

          if (!currentOwner.isTrait)
            nullables(currentOwner) = lazyValNullables(currentOwner, body)

          tree
        case dd: DefDef if dd.symbol.name.endsWith(nme.LAZY_SLOW_SUFFIX) =>
          val fieldsToNull = nullables.getOrElse(sym.enclClass, Map()).getOrElse(sym, Nil)
          if (fieldsToNull.isEmpty) dd
          else {
            deriveDefDef(dd) {
              case blk@Block(stats, expr) =>
                assert(dd.symbol.originalOwner.isClass, dd.symbol)
                def nullify(sym: Symbol) = {
                  sym.accessedOrSelf.setFlag(MUTABLE)
                  Select(gen.mkAttributedThis(sym.enclClass), sym.accessedOrSelf) === NULL
                }
                val stats1 = stats ::: fieldsToNull.map(nullify)
                treeCopy.Block(blk, stats1, expr)
              case tree =>
                devWarning("Unexpected tree shape in lazy slow path")
                tree
            }
          }

        case _ => tree
      }
    }

    /** Map lazy values to the fields they should null after initialization. */
    def lazyValNullables(clazz: Symbol, templStats: List[Tree]): Map[Symbol, List[Symbol]] = {
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

          object SingleUseTraverser extends InternalTraverser {
            override def traverse(tree: Tree): Unit = {
              tree match {
                // assignment targets don't count as a dereference -- only check the rhs
                case Assign(_, rhs) => traverse(rhs)
                case tree: RefTree if tree.symbol != NoSymbol =>
                  val sym = tree.symbol
                  // println(s"$sym in ${sym.owner} from $currentOwner ($tree)")
                  if ((sym.hasAccessorFlag || (sym.isTerm && !sym.isMethod)) && sym.isPrivate && !sym.isLazy && !sym.isModule // non-lazy private field or its accessor
                    && !definitions.isPrimitiveValueClass(sym.tpe.resultType.typeSymbol) // primitives don't hang on to significant amounts of heap
                    && sym.owner == currentOwner.enclClass && !(currentOwner.isGetter && currentOwner.accessed == sym)) {

                    // println("added use in: " + currentOwner + " -- " + tree)
                    usedIn(sym) ::= currentOwner
                  }
                  tree.traverse(this)
                case _ => tree.traverse(this)
              }
            }
          }
          templStats foreach SingleUseTraverser.apply
          // println("usedIn: " + usedIn)

          // only consider usages from non-transient lazy vals (scala/bug#9365)
          val singlyUsedIn = usedIn.filter {
            case (_, member :: Nil) if member.name.endsWith(nme.LAZY_SLOW_SUFFIX) =>
              val lazyAccessor = member.owner.info.decl(member.name.stripSuffix(nme.LAZY_SLOW_SUFFIX))
              !lazyAccessor.accessedOrSelf.hasAnnotation(TransientAttr)
            case _ => false
          }.toMap

          // println("singlyUsedIn: " + singlyUsedIn)
          singlyUsedIn
        }

        val map = mutable.Map[Symbol, Set[Symbol]]() withDefaultValue Set()
        // invert the map to see which fields can be nulled for each non-transient lazy val
        for ((field, users) <- singleUseFields; lazyFld <- users) map(lazyFld) += field

        map.view.mapValues(_.toList.sortBy(_.id)).toMap
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
    private def addNewDefs(clazz: Symbol, stats: List[Tree]): List[Tree] = {
      val accessorSynth = new UncheckedAccessorSynth(clazz)
      import accessorSynth._

      // for all symbols `sym` in the class definition, which are mixed in by mixinTraitMembers
      for (sym <- clazz.info.decls ; if sym hasFlag MIXEDIN) {
        // if current class is a trait, add an abstract method for accessor `sym`
        // ditto for a super accessor (will get an RHS in completeSuperAccessor)
        if (clazz.isTrait || sym.isSuperAccessor) addDefDef(sym)
        // implement methods mixed in from a supertrait (the symbols were created by mixinTraitMembers)
        else if (sym.hasFlag(ACCESSOR) && !sym.hasFlag(DEFERRED)) {
          assert(sym hasFlag (PARAMACCESSOR), s"mixed in $sym from $clazz is not param?!?")

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

      val implementedAccessors = implementWithNewDefs(stats)

      if (clazz.isTrait)
        implementedAccessors filter {
          case vd: ValDef => assert(vd.symbol.hasFlag(PRESUPER | PARAMACCESSOR), s"unexpected valdef $vd in trait $clazz"); false
          case _ => true
        }
      else {
        /* If `stat` is a superaccessor, complete it by adding a right-hand side.
         * Note: superaccessors are always abstract until this point.
         *  The method to call in a superaccessor is stored in the accessor symbol's alias field.
         * The rhs is:
         *   super.A(xs)  where A is the super accessor's alias and xs are its formal parameters.
         * This rhs is typed and then mixin transformed.
         */
        def completeSuperAccessor(stat: Tree) = stat match {
          case DefDef(_, _, _, vparams :: Nil, _, EmptyTree) if stat.symbol.isSuperAccessor =>
            val alias = stat.symbol.alias
            debuglog(s"implementing super accessor in $clazz for ${stat.symbol} --> ${alias.owner} . ${alias}")
            registerRequiredDirectInterface(alias, clazz, parent =>
              s"Unable to implement a super accessor, $parent needs to be directly extended by $clazz.")
            val body = atPos(stat.pos)(Apply(SuperSelect(clazz, alias), vparams map (v => Ident(v.symbol))))
            val pt   = stat.symbol.tpe.resultType

            copyDefDef(stat)(rhs = enteringMixin(transform(localTyper.typed(body, pt))))
          case _ =>
            stat
        }

        implementedAccessors map completeSuperAccessor
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
        case Template(parents, self, body) =>
          // change parents of templates to conform to parents in the symbol info
          val parents1 = currentOwner.info.parents map (t => TypeTree(t) setPos tree.pos)

          // add all new definitions to current class or interface
          val statsWithNewDefs = addNewDefs(currentOwner, body)
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
      val tree1 = preTransform(tree).transform(this)
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
