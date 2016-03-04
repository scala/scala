/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

import symtab.Flags._
import scala.collection.{ mutable, immutable }
import transform.InfoTransform
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.tools.nsc.settings.ScalaVersion
import scala.tools.nsc.settings.AnyScalaVersion
import scala.tools.nsc.settings.NoScalaVersion

/** <p>
 *    Post-attribution checking and transformation.
 *  </p>
 *  <p>
 *    This phase performs the following checks.
 *  </p>
 *  <ul>
 *    <li>All overrides conform to rules.</li>
 *    <li>All type arguments conform to bounds.</li>
 *    <li>All type variable uses conform to variance annotations.</li>
 *    <li>No forward reference to a term symbol extends beyond a value definition.</li>
 *  </ul>
 *  <p>
 *    It performs the following transformations.
 *  </p>
 *  <ul>
 *   <li>Local modules are replaced by variables and classes</li>
 *   <li>Calls to case factory methods are replaced by new's.</li>
 *   <li>Eliminate branches in a conditional if the condition is a constant</li>
 *  </ul>
 *
 *  @author  Martin Odersky
 *  @version 1.0
 *
 *  @todo    Check whether we always check type parameter bounds.
 */
abstract class RefChecks extends InfoTransform with scala.reflect.internal.transform.RefChecks {

  val global: Global               // need to repeat here because otherwise last mixin defines global as
                                   // SymbolTable. If we had DOT this would not be an issue

  import global._
  import definitions._
  import typer.{typed, typedOperator, atOwner}

  /** the following two members override abstract members in Transform */
  val phaseName: String = "refchecks"
  override def phaseNewFlags: Long = lateMETHOD

  def newTransformer(unit: CompilationUnit): RefCheckTransformer =
    new RefCheckTransformer(unit)
  override def changesBaseClasses = false

  override def transformInfo(sym: Symbol, tp: Type): Type = {
    // !!! This is a sketchy way to do things.
    // It would be better to replace the module symbol with a method symbol
    // rather than creating this module/method hybrid which must be special
    // cased all over the place. Look for the call sites which use(d) some
    // variation of "isMethod && !isModule", which to an observer looks like
    // a nonsensical condition. (It is now "isModuleNotMethod".)
    if (sym.isModule && !sym.isStatic) {
      sym setFlag lateMETHOD | STABLE
      // Note that this as far as we can see it works equally well
      // to set the METHOD flag here and dump lateMETHOD, but it does
      // mean that under separate compilation the typer will see
      // modules as methods (albeit stable ones with singleton types.)
      // So for now lateMETHOD lives while we try to convince ourselves
      // we can live without it or deliver that info some other way.
      log(s"Stabilizing module method for ${sym.fullLocationString}")
    }
    super.transformInfo(sym, tp)
  }

  val toJavaRepeatedParam  = new SubstSymMap(RepeatedParamClass -> JavaRepeatedParamClass)
  val toScalaRepeatedParam = new SubstSymMap(JavaRepeatedParamClass -> RepeatedParamClass)

  def accessFlagsToString(sym: Symbol) = flagsToString(
    sym getFlag (PRIVATE | PROTECTED),
    if (sym.hasAccessBoundary) "" + sym.privateWithin.name else ""
  )

  def overridesTypeInPrefix(tp1: Type, tp2: Type, prefix: Type): Boolean = (tp1.dealiasWiden, tp2.dealiasWiden) match {
    case (MethodType(List(), rtp1), NullaryMethodType(rtp2)) =>
      rtp1 <:< rtp2
    case (NullaryMethodType(rtp1), MethodType(List(), rtp2)) =>
      rtp1 <:< rtp2
    case (TypeRef(_, sym, _),  _) if sym.isModuleClass =>
      overridesTypeInPrefix(NullaryMethodType(tp1), tp2, prefix)
    case _ =>
      def classBoundAsSeen(tp: Type) = tp.typeSymbol.classBound.asSeenFrom(prefix, tp.typeSymbol.owner)

      (tp1 <:< tp2) || (  // object override check
        tp1.typeSymbol.isModuleClass && tp2.typeSymbol.isModuleClass && {
          val cb1 = classBoundAsSeen(tp1)
          val cb2 = classBoundAsSeen(tp2)
          (cb1 <:< cb2) && {
            log("Allowing %s to override %s because %s <:< %s".format(tp1, tp2, cb1, cb2))
            true
          }
        }
      )
  }

  class RefCheckTransformer(unit: CompilationUnit) extends Transformer {

    var localTyper: analyzer.Typer = typer
    var currentApplication: Tree = EmptyTree
    var inPattern: Boolean = false
    @inline final def savingInPattern[A](body: => A): A = {
      val saved = inPattern
      try body finally inPattern = saved
    }

    var checkedCombinations = Set[List[Type]]()

    // only one overloaded alternative is allowed to define default arguments
    private def checkOverloadedRestrictions(clazz: Symbol, defaultClass: Symbol): Unit = {
      // Using the default getters (such as methodName$default$1) as a cheap way of
      // finding methods with default parameters. This way, we can limit the members to
      // those with the DEFAULTPARAM flag, and infer the methods. Looking for the methods
      // directly requires inspecting the parameter list of every one. That modification
      // shaved 95% off the time spent in this method.
      val defaultGetters     = defaultClass.info.findMembers(excludedFlags = PARAM, requiredFlags = DEFAULTPARAM)
      val defaultMethodNames = defaultGetters map (sym => nme.defaultGetterToMethod(sym.name))

      defaultMethodNames.toList.distinct foreach { name =>
        val methods      = clazz.info.findMember(name, 0L, requiredFlags = METHOD, stableOnly = false).alternatives
        def hasDefaultParam(tpe: Type): Boolean = tpe match {
          case MethodType(params, restpe) => (params exists (_.hasDefault)) || hasDefaultParam(restpe)
          case _                          => false
        }
        val haveDefaults = methods filter (
          if (settings.isScala211)
             (sym => mexists(sym.info.paramss)(_.hasDefault) && !nme.isProtectedAccessorName(sym.name))
          else
            (sym => hasDefaultParam(sym.info) && !nme.isProtectedAccessorName(sym.name))
        )

        if (haveDefaults.lengthCompare(1) > 0) {
          val owners = haveDefaults map (_.owner)
           // constructors of different classes are allowed to have defaults
          if (haveDefaults.exists(x => !x.isConstructor) || owners.distinct.size < haveDefaults.size) {
            reporter.error(clazz.pos,
              "in "+ clazz +
              ", multiple overloaded alternatives of "+ haveDefaults.head +
              " define default arguments" + (
                if (owners.forall(_ == clazz)) "."
                else ".\nThe members with defaults are defined in "+owners.map(_.fullLocationString).mkString("", " and ", ".")
              )
            )
          }
        }
      }

      // Check for doomed attempt to overload applyDynamic
      if (clazz isSubClass DynamicClass) {
        for ((_, m1 :: m2 :: _) <- (clazz.info member nme.applyDynamic).alternatives groupBy (_.typeParams.length)) {
          reporter.error(m1.pos, "implementation restriction: applyDynamic cannot be overloaded except by methods with different numbers of type parameters, e.g. applyDynamic[T1](method: String)(arg: T1) and applyDynamic[T1, T2](method: String)(arg1: T1, arg2: T2)")
        }
      }

      // This has become noisy with implicit classes.
      if (settings.warnPolyImplicitOverload && settings.developer) {
        clazz.info.decls filter (x => x.isImplicit && x.typeParams.nonEmpty) foreach { sym =>
          // implicit classes leave both a module symbol and a method symbol as residue
          val alts = clazz.info.decl(sym.name).alternatives filterNot (_.isModule)
          if (alts.size > 1)
            alts foreach (x => reporter.warning(x.pos, "parameterized overloaded implicit methods are not visible as view bounds"))
        }
      }
    }

// Override checking ------------------------------------------------------------

    /** Add bridges for vararg methods that extend Java vararg methods
     */
    def addVarargBridges(clazz: Symbol): List[Tree] = {
      // This is quite expensive, so attempt to skip it completely.
      // Insist there at least be a java-defined ancestor which
      // defines a varargs method. TODO: Find a cheaper way to exclude.
      if (inheritsJavaVarArgsMethod(clazz)) {
        log("Found java varargs ancestor in " + clazz.fullLocationString + ".")
        val self = clazz.thisType
        val bridges = new ListBuffer[Tree]

        def varargBridge(member: Symbol, bridgetpe: Type): Tree = {
          log(s"Generating varargs bridge for ${member.fullLocationString} of type $bridgetpe")

          val newFlags = (member.flags | VBRIDGE | ARTIFACT) & ~PRIVATE
          val bridge   = member.cloneSymbolImpl(clazz, newFlags) setPos clazz.pos
          bridge.setInfo(bridgetpe.cloneInfo(bridge))
          clazz.info.decls enter bridge

          val params  = bridge.paramss.head
          val elemtp  = params.last.tpe.typeArgs.head
          val idents  = params map Ident
          val lastarg = gen.wildcardStar(gen.mkWrapArray(idents.last, elemtp))
          val body    = Apply(Select(This(clazz), member), idents.init :+ lastarg)

          localTyper typed DefDef(bridge, body)
        }

        // For all concrete non-private members (but: see below) that have a (Scala) repeated
        //   parameter: compute the corresponding method type `jtpe` with a Java repeated parameter
        //   if a method with type `jtpe` exists and that method is not a varargs bridge
        //   then create a varargs bridge of type `jtpe` that forwards to the
        //   member method with the Scala vararg type.
        //
        // @PP: Can't call nonPrivateMembers because we will miss refinement members,
        //   which have been marked private. See SI-4729.
        for (member <- nonTrivialMembers(clazz)) {
          log(s"Considering $member for java varargs bridge in $clazz")
          if (!member.isDeferred && member.isMethod && hasRepeatedParam(member.info)) {
            val inherited = clazz.info.nonPrivateMemberAdmitting(member.name, VBRIDGE)

            // Delaying calling memberType as long as possible
            if (inherited.exists) {
              val jtpe = toJavaRepeatedParam(self memberType member)
              // this is a bit tortuous: we look for non-private members or bridges
              // if we find a bridge everything is OK. If we find another member,
              // we need to create a bridge
              val inherited1 = inherited filter (sym => !(sym hasFlag VBRIDGE) && (self memberType sym matches jtpe))
              if (inherited1.exists)
                bridges += varargBridge(member, jtpe)
            }
          }
        }

        if (bridges.size > 0)
          log(s"Adding ${bridges.size} bridges for methods extending java varargs.")

        bridges.toList
      }
      else Nil
    }

    /** 1. Check all members of class `clazz` for overriding conditions.
     *  That is for overriding member M and overridden member O:
     *
     *    1.1. M must have the same or stronger access privileges as O.
     *    1.2. O must not be final.
     *    1.3. O is deferred, or M has `override` modifier.
     *    1.4. If O is stable, then so is M.
     *     // @M: LIFTED 1.5. Neither M nor O are a parameterized type alias
     *    1.6. If O is a type alias, then M is an alias of O.
     *    1.7. If O is an abstract type then
     *       1.7.1 either M is an abstract type, and M's bounds are sharper than O's bounds.
     *             or M is a type alias or class which conforms to O's bounds.
     *       1.7.2 higher-order type arguments must respect bounds on higher-order type parameters  -- @M
     *              (explicit bounds and those implied by variance annotations) -- @see checkKindBounds
     *    1.8. If O and M are values, then
     *    1.8.1  M's type is a subtype of O's type, or
     *    1.8.2  M is of type []S, O is of type ()T and S <: T, or
     *    1.8.3  M is of type ()S, O is of type []T and S <: T, or
     *    1.9.  If M is a macro def, O cannot be deferred unless there's a concrete method overriding O.
     *    1.10. If M is not a macro def, O cannot be a macro def.
     *  2. Check that only abstract classes have deferred members
     *  3. Check that concrete classes do not have deferred definitions
     *     that are not implemented in a subclass.
     *  4. Check that every member with an `override` modifier
     *     overrides some other member.
     */
    private def checkAllOverrides(clazz: Symbol, typesOnly: Boolean = false) {
      val self = clazz.thisType
      def classBoundAsSeen(tp: Type) = {
        tp.typeSymbol.classBound.asSeenFrom(self, tp.typeSymbol.owner)
      }

      case class MixinOverrideError(member: Symbol, msg: String)

      val mixinOverrideErrors = new ListBuffer[MixinOverrideError]()

      def printMixinOverrideErrors() {
        mixinOverrideErrors.toList match {
          case List() =>
          case List(MixinOverrideError(_, msg)) =>
            reporter.error(clazz.pos, msg)
          case MixinOverrideError(member, msg) :: others =>
            val others1 = others.map(_.member.name.decode).filter(member.name.decode != _).distinct
            reporter.error(
              clazz.pos,
              msg+(if (others1.isEmpty) ""
                   else ";\n other members with override errors are: "+(others1 mkString ", ")))
        }
      }

      def infoString(sym: Symbol) = infoString0(sym, sym.owner != clazz)
      def infoStringWithLocation(sym: Symbol) = infoString0(sym, true)

      def infoString0(sym: Symbol, showLocation: Boolean) = {
        val sym1 = analyzer.underlyingSymbol(sym)
        sym1.toString() +
        (if (showLocation)
          sym1.locationString +
          (if (sym1.isAliasType) ", which equals "+self.memberInfo(sym1)
           else if (sym1.isAbstractType) " with bounds"+self.memberInfo(sym1)
           else if (sym1.isModule) ""
           else if (sym1.isTerm) " of type "+self.memberInfo(sym1)
           else "")
         else "")
      }

      /* Check that all conditions for overriding `other` by `member`
       * of class `clazz` are met.
       */
      def checkOverride(pair: SymbolPair) {
        import pair._
        val member   = low
        val other    = high
        def memberTp = lowType
        def otherTp  = highType

        debuglog("Checking validity of %s overriding %s".format(member.fullLocationString, other.fullLocationString))

        def noErrorType = !pair.isErroneous
        def isRootOrNone(sym: Symbol) = sym != null && sym.isRoot || sym == NoSymbol
        def isNeitherInClass = member.owner != pair.base && other.owner != pair.base

        def objectOverrideErrorMsg = (
          "overriding " + high.fullLocationString + " with " + low.fullLocationString + ":\n" +
          "an overriding object must conform to the overridden object's class bound" +
          analyzer.foundReqMsg(pair.lowClassBound, pair.highClassBound)
        )

        def overrideErrorMsg(msg: String): String = {
          val isConcreteOverAbstract =
            (other.owner isSubClass member.owner) && other.isDeferred && !member.isDeferred
          val addendum =
            if (isConcreteOverAbstract)
              ";\n (Note that %s is abstract,\n  and is therefore overridden by concrete %s)".format(
                infoStringWithLocation(other),
                infoStringWithLocation(member)
              )
            else if (settings.debug)
              analyzer.foundReqMsg(member.tpe, other.tpe)
            else ""

          "overriding %s;\n %s %s%s".format(
            infoStringWithLocation(other), infoString(member), msg, addendum
          )
        }
        def emitOverrideError(fullmsg: String) {
          if (member.owner == clazz) reporter.error(member.pos, fullmsg)
          else mixinOverrideErrors += new MixinOverrideError(member, fullmsg)
        }

        def overrideError(msg: String) {
          if (noErrorType)
            emitOverrideError(overrideErrorMsg(msg))
        }

        def overrideTypeError() {
          if (noErrorType) {
            emitOverrideError(
              if (member.isModule && other.isModule) objectOverrideErrorMsg
              else overrideErrorMsg("has incompatible type")
            )
          }
        }

        def overrideAccessError() {
          val otherAccess = accessFlagsToString(other)
          overrideError("has weaker access privileges; it should be "+ (if (otherAccess == "") "public" else "at least "+otherAccess))
        }

        //Console.println(infoString(member) + " overrides " + infoString(other) + " in " + clazz);//DEBUG

        // return if we already checked this combination elsewhere
        if (member.owner != clazz) {
          def deferredCheck        = member.isDeferred || !other.isDeferred
          def subOther(s: Symbol)  = s isSubClass other.owner
          def subMember(s: Symbol) = s isSubClass member.owner

          if (subOther(member.owner) && deferredCheck) {
            //Console.println(infoString(member) + " shadows1 " + infoString(other) " in " + clazz);//DEBUG
            return
          }
          if (clazz.parentSymbols exists (p => subOther(p) && subMember(p) && deferredCheck)) {
            //Console.println(infoString(member) + " shadows2 " + infoString(other) + " in " + clazz);//DEBUG
            return
          }
          if (clazz.parentSymbols forall (p => subOther(p) == subMember(p))) {
            //Console.println(infoString(member) + " shadows " + infoString(other) + " in " + clazz);//DEBUG
            return
          }
        }

        /* Is the intersection between given two lists of overridden symbols empty? */
        def intersectionIsEmpty(syms1: List[Symbol], syms2: List[Symbol]) =
          !(syms1 exists (syms2 contains _))

        if (typesOnly) checkOverrideTypes()
        else {
          // o: public | protected        | package-protected  (aka java's default access)
          // ^-may be overridden by member with access privileges-v
          // m: public | public/protected | public/protected/package-protected-in-same-package-as-o

          if (member.isPrivate) // (1.1)
            overrideError("has weaker access privileges; it should not be private")

          // todo: align accessibility implication checking with isAccessible in Contexts
          val ob = other.accessBoundary(member.owner)
          val mb = member.accessBoundary(member.owner)
          def isOverrideAccessOK = member.isPublic || {      // member is public, definitely same or relaxed access
            (!other.isProtected || member.isProtected) &&    // if o is protected, so is m
            ((!isRootOrNone(ob) && ob.hasTransOwner(mb)) ||  // m relaxes o's access boundary
              other.isJavaDefined)                           // overriding a protected java member, see #3946
          }
          if (!isOverrideAccessOK) {
            overrideAccessError()
          } else if (other.isClass) {
            overrideError("cannot be used here - class definitions cannot be overridden")
          } else if (!other.isDeferred && member.isClass) {
            overrideError("cannot be used here - classes can only override abstract types")
          } else if (other.isEffectivelyFinal) { // (1.2)
            overrideError("cannot override final member")
          } else if (!other.isDeferredOrJavaDefault && !other.hasFlag(JAVA_DEFAULTMETHOD) && !member.isAnyOverride && !member.isSynthetic) { // (*)
            // (*) Synthetic exclusion for (at least) default getters, fixes SI-5178. We cannot assign the OVERRIDE flag to
            // the default getter: one default getter might sometimes override, sometimes not. Example in comment on ticket.
              if (isNeitherInClass && !(other.owner isSubClass member.owner))
                emitOverrideError(
                  clazz + " inherits conflicting members:\n  "
                    + infoStringWithLocation(other) + "  and\n  " + infoStringWithLocation(member)
                    + "\n(Note: this can be resolved by declaring an override in " + clazz + ".)"
                )
              else
                overrideError("needs `override' modifier")
          } else if (other.isAbstractOverride && other.isIncompleteIn(clazz) && !member.isAbstractOverride) {
            overrideError("needs `abstract override' modifiers")
          }
          else if (member.isAnyOverride && (other hasFlag ACCESSOR) && other.accessed.isVariable && !other.accessed.isLazy) {
            // !?! this is not covered by the spec. We need to resolve this either by changing the spec or removing the test here.
            // !!! is there a !?! convention? I'm !!!ing this to make sure it turns up on my searches.
            if (!settings.overrideVars)
              overrideError("cannot override a mutable variable")
          }
          else if (member.isAnyOverride &&
                     !(member.owner.thisType.baseClasses exists (_ isSubClass other.owner)) &&
                     !member.isDeferred && !other.isDeferred &&
                     intersectionIsEmpty(member.extendedOverriddenSymbols, other.extendedOverriddenSymbols)) {
            overrideError("cannot override a concrete member without a third member that's overridden by both "+
                          "(this rule is designed to prevent ``accidental overrides'')")
          } else if (other.isStable && !member.isStable) { // (1.4)
            overrideError("needs to be a stable, immutable value")
          } else if (member.isValue && member.isLazy &&
                     other.isValue && !other.isSourceMethod && !other.isDeferred && !other.isLazy) {
            overrideError("cannot override a concrete non-lazy value")
          } else if (other.isValue && other.isLazy && !other.isSourceMethod && !other.isDeferred &&
                     member.isValue && !member.isLazy) {
            overrideError("must be declared lazy to override a concrete lazy value")
          } else if (other.isDeferred && member.isTermMacro && member.extendedOverriddenSymbols.forall(_.isDeferred)) { // (1.9)
            overrideError("cannot be used here - term macros cannot override abstract methods")
          } else if (other.isTermMacro && !member.isTermMacro) { // (1.10)
            overrideError("cannot be used here - only term macros can override term macros")
          } else {
            checkOverrideTypes()
            checkOverrideDeprecated()
            if (settings.warnNullaryOverride) {
              if (other.paramss.isEmpty && !member.paramss.isEmpty) {
                reporter.warning(member.pos, "non-nullary method overrides nullary method")
              }
            }
          }
        }

        //if (!member.typeParams.isEmpty) (1.5)  @MAT
        //  overrideError("may not be parameterized");
        //if (!other.typeParams.isEmpty)  (1.5)   @MAT
        //  overrideError("may not override parameterized type");
        // @M: substSym
        def checkOverrideAlias() {
          // Important: first check the pair has the same kind, since the substitution
          // carries high's type parameter's bounds over to low, so that
          // type equality doesn't consider potentially different bounds on low/high's type params.
          // In b781e25afe this went from using memberInfo to memberType (now lowType/highType), tested by neg/override.scala.
          // TODO: was that the right fix? it seems type alias's RHS should be checked by looking at the symbol's info
          if (pair.sameKind && lowType.substSym(low.typeParams, high.typeParams) =:= highType) ()
          else overrideTypeError() // (1.6)
        }
        //if (!member.typeParams.isEmpty) // (1.7)  @MAT
        //  overrideError("may not be parameterized");
        def checkOverrideAbstract() {
          if (!(highInfo.bounds containsType lowType)) { // (1.7.1)
            overrideTypeError(); // todo: do an explaintypes with bounds here
            explainTypes(_.bounds containsType _, highInfo, lowType)
          }
          // check overriding (abstract type --> abstract type or abstract type --> concrete type member (a type alias))
          // making an abstract type member concrete is like passing a type argument
          typer.infer.checkKindBounds(high :: Nil, lowType :: Nil, rootType, low.owner) match { // (1.7.2)
            case Nil        =>
            case kindErrors =>
              reporter.error(member.pos,
                "The kind of "+member.keyString+" "+member.varianceString + member.nameString+
                " does not conform to the expected kind of " + other.defString + other.locationString + "." +
                kindErrors.toList.mkString("\n", ", ", ""))
          }
          // check a type alias's RHS corresponds to its declaration
          // this overlaps somewhat with validateVariance
          if (low.isAliasType) {
            typer.infer.checkKindBounds(low :: Nil, lowType.normalize :: Nil, rootType, low.owner) match {
              case Nil        =>
              case kindErrors =>
                reporter.error(member.pos,
                  "The kind of the right-hand side "+lowType.normalize+" of "+low.keyString+" "+
                  low.varianceString + low.nameString+ " does not conform to its expected kind."+
                  kindErrors.toList.mkString("\n", ", ", ""))
            }
          }
          else if (low.isAbstractType && lowType.isVolatile && !highInfo.bounds.hi.isVolatile)
            overrideError("is a volatile type; cannot override a type with non-volatile upper bound")
        }
        def checkOverrideTerm() {
          other.cookJavaRawInfo() // #2454
          if (!overridesTypeInPrefix(lowType, highType, rootType)) { // 8
            overrideTypeError()
            explainTypes(lowType, highType)
          }
          if (low.isStable && !highType.isVolatile) {
            if (lowType.isVolatile)
              overrideError("has a volatile type; cannot override a member with non-volatile type")
            else lowType.normalize.resultType match {
              case rt: RefinedType if !(rt =:= highType) && !(checkedCombinations contains rt.parents) =>
                // might mask some inconsistencies -- check overrides
                checkedCombinations += rt.parents
                val tsym = rt.typeSymbol
                if (tsym.pos == NoPosition) tsym setPos member.pos
                checkAllOverrides(tsym, typesOnly = true)
              case _ =>
            }
          }
        }
        def checkOverrideTypes() {
          if (high.isAliasType)         checkOverrideAlias()
          else if (high.isAbstractType) checkOverrideAbstract()
          else if (high.isTerm)         checkOverrideTerm()
        }

        def checkOverrideDeprecated() {
          if (other.hasDeprecatedOverridingAnnotation && !member.ownerChain.exists(x => x.isDeprecated || x.hasBridgeAnnotation)) {
            val suffix = other.deprecatedOverridingMessage map (": " + _) getOrElse ""
            val msg = s"overriding ${other.fullLocationString} is deprecated$suffix"
            currentRun.reporting.deprecationWarning(member.pos, other, msg)
          }
        }
      }

      val opc = new overridingPairs.Cursor(clazz)
      while (opc.hasNext) {
        if (!opc.high.isClass)
          checkOverride(opc.currentPair)

        opc.next()
      }
      printMixinOverrideErrors()

      // Verifying a concrete class has nothing unimplemented.
      if (clazz.isConcreteClass && !typesOnly) {
        val abstractErrors = new ListBuffer[String]
        def abstractErrorMessage =
          // a little formatting polish
          if (abstractErrors.size <= 2) abstractErrors mkString " "
          else abstractErrors.tail.mkString(abstractErrors.head + ":\n", "\n", "")

        def abstractClassError(mustBeMixin: Boolean, msg: String) {
          def prelude = (
            if (clazz.isAnonymousClass || clazz.isModuleClass) "object creation impossible"
            else if (mustBeMixin) clazz + " needs to be a mixin"
            else clazz + " needs to be abstract"
          ) + ", since"

          if (abstractErrors.isEmpty) abstractErrors ++= List(prelude, msg)
          else abstractErrors += msg
        }

        def javaErasedOverridingSym(sym: Symbol): Symbol =
          clazz.tpe.nonPrivateMemberAdmitting(sym.name, BRIDGE).filter(other =>
            !other.isDeferred && other.isJavaDefined && !sym.enclClass.isSubClass(other.enclClass) && {
              // #3622: erasure operates on uncurried types --
              // note on passing sym in both cases: only sym.isType is relevant for uncurry.transformInfo
              // !!! erasure.erasure(sym, uncurry.transformInfo(sym, tp)) gives erroneous or inaccessible type - check whether that's still the case!
              def uncurryAndErase(tp: Type) = erasure.erasure(sym)(uncurry.transformInfo(sym, tp))
              val tp1 = uncurryAndErase(clazz.thisType.memberType(sym))
              val tp2 = uncurryAndErase(clazz.thisType.memberType(other))
              exitingErasure(tp1 matches tp2)
            })

        def ignoreDeferred(member: Symbol) = (
          (member.isAbstractType && !member.isFBounded) || (
            // the test requires exitingErasure so shouldn't be
            // done if the compiler has no erasure phase available
               member.isJavaDefined
            && (currentRun.erasurePhase == NoPhase || javaErasedOverridingSym(member) != NoSymbol)
          )
        )

        // 2. Check that only abstract classes have deferred members
        def checkNoAbstractMembers(): Unit = {
          // Avoid spurious duplicates: first gather any missing members.
          def memberList = clazz.info.nonPrivateMembersAdmitting(VBRIDGE)
          val (missing, rest) = memberList partition (m => m.isDeferredNotJavaDefault && !ignoreDeferred(m))
          // Group missing members by the name of the underlying symbol,
          // to consolidate getters and setters.
          val grouped = missing groupBy (sym => analyzer.underlyingSymbol(sym).name)
          val missingMethods = grouped.toList flatMap {
            case (name, syms) =>
              if (syms exists (_.isSetter)) syms filterNot (_.isGetter)
              else syms
          }

          def stubImplementations: List[String] = {
            // Grouping missing methods by the declaring class
            val regrouped = missingMethods.groupBy(_.owner).toList
            def membersStrings(members: List[Symbol]) = {
              members foreach fullyInitializeSymbol
              members.sortBy(_.name) map (m => m.defStringSeenAs(clazz.tpe_* memberType m) + " = ???")
            }

            if (regrouped.tail.isEmpty)
              membersStrings(regrouped.head._2)
            else (regrouped.sortBy("" + _._1.name) flatMap {
              case (owner, members) =>
                ("// Members declared in " + owner.fullName) +: membersStrings(members) :+ ""
            }).init
          }

          // If there are numerous missing methods, we presume they are aware of it and
          // give them a nicely formatted set of method signatures for implementing.
          if (missingMethods.size > 1) {
            abstractClassError(false, "it has " + missingMethods.size + " unimplemented members.")
            val preface =
              """|/** As seen from %s, the missing signatures are as follows.
                 | *  For convenience, these are usable as stub implementations.
                 | */
                 |""".stripMargin.format(clazz)
            abstractErrors += stubImplementations.map("  " + _ + "\n").mkString(preface, "", "")
            return
          }

          for (member <- missing) {
            def undefined(msg: String) = abstractClassError(false, infoString(member) + " is not defined" + msg)
            val underlying = analyzer.underlyingSymbol(member)

            // Give a specific error message for abstract vars based on why it fails:
            // It could be unimplemented, have only one accessor, or be uninitialized.
            if (underlying.isVariable) {
              val isMultiple = grouped.getOrElse(underlying.name, Nil).size > 1

              // If both getter and setter are missing, squelch the setter error.
              if (member.isSetter && isMultiple) ()
              else undefined(
                if (member.isSetter) "\n(Note that an abstract var requires a setter in addition to the getter)"
                else if (member.isGetter && !isMultiple) "\n(Note that an abstract var requires a getter in addition to the setter)"
                else analyzer.abstractVarMessage(member)
              )
            }
            else if (underlying.isMethod) {
              // If there is a concrete method whose name matches the unimplemented
              // abstract method, and a cursory examination of the difference reveals
              // something obvious to us, let's make it more obvious to them.
              val abstractParams   = underlying.tpe.paramTypes
              val matchingName     = clazz.tpe.nonPrivateMembersAdmitting(VBRIDGE)
              val matchingArity    = matchingName filter { m =>
                !m.isDeferred &&
                (m.name == underlying.name) &&
                (m.tpe.paramTypes.size == underlying.tpe.paramTypes.size) &&
                (m.tpe.typeParams.size == underlying.tpe.typeParams.size)
              }

              matchingArity match {
                // So far so good: only one candidate method
                case Scope(concrete)   =>
                  val mismatches  = abstractParams zip concrete.tpe.paramTypes filterNot { case (x, y) => x =:= y }
                  mismatches match {
                    // Only one mismatched parameter: say something useful.
                    case (pa, pc) :: Nil  =>
                      val abstractSym = pa.typeSymbol
                      val concreteSym = pc.typeSymbol
                      def subclassMsg(c1: Symbol, c2: Symbol) = (
                        ": %s is a subclass of %s, but method parameter types must match exactly.".format(
                          c1.fullLocationString, c2.fullLocationString)
                      )
                      val addendum = (
                        if (abstractSym == concreteSym) {
                          // TODO: what is the optimal way to test for a raw type at this point?
                          // Compilation has already failed so we shouldn't have to worry overmuch
                          // about forcing types.
                          if (underlying.isJavaDefined && pa.typeArgs.isEmpty && abstractSym.typeParams.nonEmpty)
                            ". To implement a raw type, use %s[_]".format(pa)
                          else if (pa.prefix =:= pc.prefix)
                            ": their type parameters differ"
                          else
                            ": their prefixes (i.e. enclosing instances) differ"
                        }
                        else if (abstractSym isSubClass concreteSym)
                          subclassMsg(abstractSym, concreteSym)
                        else if (concreteSym isSubClass abstractSym)
                          subclassMsg(concreteSym, abstractSym)
                        else ""
                      )

                      undefined("\n(Note that %s does not match %s%s)".format(pa, pc, addendum))
                    case xs =>
                      undefined("")
                  }
                case _ =>
                  undefined("")
              }
            }
            else undefined("")
          }

          // Check the remainder for invalid absoverride.
          for (member <- rest ; if (member.isAbstractOverride && member.isIncompleteIn(clazz))) {
            val other = member.superSymbolIn(clazz)
            val explanation =
              if (other != NoSymbol) " and overrides incomplete superclass member " + infoString(other)
              else ", but no concrete implementation could be found in a base class"

            abstractClassError(true, infoString(member) + " is marked `abstract' and `override'" + explanation)
          }
        }

        // 3. Check that concrete classes do not have deferred definitions
        // that are not implemented in a subclass.
        // Note that this is not the same as (2); In a situation like
        //
        // class C { def m: Int = 0}
        // class D extends C { def m: Int }
        //
        // (3) is violated but not (2).
        def checkNoAbstractDecls(bc: Symbol) {
          for (decl <- bc.info.decls) {
            if (decl.isDeferred && !ignoreDeferred(decl)) {
              val impl = decl.matchingSymbol(clazz.thisType, admit = VBRIDGE)
              if (impl == NoSymbol || (decl.owner isSubClass impl.owner)) {
                abstractClassError(false, "there is a deferred declaration of "+infoString(decl)+
                                   " which is not implemented in a subclass"+analyzer.abstractVarMessage(decl))
              }
            }
          }
          if (bc.superClass hasFlag ABSTRACT)
            checkNoAbstractDecls(bc.superClass)
        }

        checkNoAbstractMembers()
        if (abstractErrors.isEmpty)
          checkNoAbstractDecls(clazz)

        if (abstractErrors.nonEmpty)
          reporter.error(clazz.pos, abstractErrorMessage)
      }
      else if (clazz.isTrait && !(clazz isSubClass AnyValClass)) {
        // For non-AnyVal classes, prevent abstract methods in interfaces that override
        // final members in Object; see #4431
        for (decl <- clazz.info.decls) {
          // Have to use matchingSymbol, not a method involving overridden symbols,
          // because the scala type system understands that an abstract method here does not
          // override a concrete method in Object. The jvm, however, does not.
          val overridden = decl.matchingSymbol(ObjectClass, ObjectTpe)
          if (overridden.isFinal)
            reporter.error(decl.pos, "trait cannot redefine final method from class AnyRef")
        }
      }

      /* Returns whether there is a symbol declared in class `inclazz`
       * (which must be different from `clazz`) whose name and type
       * seen as a member of `class.thisType` matches `member`'s.
       */
      def hasMatchingSym(inclazz: Symbol, member: Symbol): Boolean = {
        val isVarargs = hasRepeatedParam(member.tpe)
        lazy val varargsType = toJavaRepeatedParam(member.tpe)

        def isSignatureMatch(sym: Symbol) = !sym.isTerm || {
          val symtpe            = clazz.thisType memberType sym
          def matches(tp: Type) = tp matches symtpe

          matches(member.tpe) || (isVarargs && matches(varargsType))
        }
        /* The rules for accessing members which have an access boundary are more
         * restrictive in java than scala.  Since java has no concept of package nesting,
         * a member with "default" (package-level) access can only be accessed by members
         * in the exact same package.  Example:
         *
         *   package a.b;
         *   public class JavaClass { void foo() { } }
         *
         * The member foo() can be accessed only from members of package a.b, and not
         * nested packages like a.b.c.  In the analogous scala class:
         *
         *   package a.b
         *   class ScalaClass { private[b] def foo() = () }
         *
         * The member IS accessible to classes in package a.b.c.  The javaAccessCheck logic
         * is restricting the set of matching signatures according to the above semantics.
         */
        def javaAccessCheck(sym: Symbol) = (
             !inclazz.isJavaDefined                             // not a java defined member
          || !sym.hasAccessBoundary                             // no access boundary
          || sym.isProtected                                    // marked protected in java, thus accessible to subclasses
          || sym.privateWithin == member.enclosingPackageClass  // exact package match
        )
        def classDecls   = inclazz.info.nonPrivateDecl(member.name)
        def matchingSyms = classDecls filter (sym => isSignatureMatch(sym) && javaAccessCheck(sym))

        (inclazz != clazz) && (matchingSyms != NoSymbol)
      }

      // 4. Check that every defined member with an `override` modifier overrides some other member.
      for (member <- clazz.info.decls)
        if (member.isAnyOverride && !(clazz.thisType.baseClasses exists (hasMatchingSym(_, member)))) {
          // for (bc <- clazz.info.baseClasses.tail) Console.println("" + bc + " has " + bc.info.decl(member.name) + ":" + bc.info.decl(member.name).tpe);//DEBUG

          val nonMatching: List[Symbol] = clazz.info.member(member.name).alternatives.filterNot(_.owner == clazz).filterNot(_.isFinal)
          def issueError(suffix: String) = reporter.error(member.pos, member.toString() + " overrides nothing" + suffix)
          nonMatching match {
            case Nil =>
              issueError("")
            case ms =>
              val superSigs = ms.map(m => m.defStringSeenAs(clazz.tpe memberType m)).mkString("\n")
              issueError(s".\nNote: the super classes of ${member.owner} contain the following, non final members named ${member.name}:\n${superSigs}")
          }
          member resetFlag (OVERRIDE | ABSOVERRIDE)  // Any Override
        }
    }

  // Basetype Checking --------------------------------------------------------

    /** <ol>
     *    <li> <!-- 1 -->
     *      Check that later type instances in the base-type sequence
     *      are subtypes of earlier type instances of the same mixin.
     *    </li>
     *  </ol>
     */
    private def validateBaseTypes(clazz: Symbol) {
      val seenParents = mutable.HashSet[Type]()
      val seenTypes = new Array[List[Type]](clazz.info.baseTypeSeq.length)
      for (i <- 0 until seenTypes.length)
        seenTypes(i) = Nil

      /* validate all base types of a class in reverse linear order. */
      def register(tp: Type): Unit = {
//        if (clazz.fullName.endsWith("Collection.Projection"))
//            println("validate base type "+tp)
        val baseClass = tp.typeSymbol
        if (baseClass.isClass) {
          val index = clazz.info.baseTypeIndex(baseClass)
          if (index >= 0) {
            if (seenTypes(index) forall (tp1 => !(tp1 <:< tp)))
              seenTypes(index) =
                tp :: (seenTypes(index) filter (tp1 => !(tp <:< tp1)))
          }
        }
        val remaining = tp.parents filterNot seenParents
        seenParents ++= remaining
        remaining foreach register
      }
      register(clazz.tpe)
      for (i <- 0 until seenTypes.length) {
        val baseClass = clazz.info.baseTypeSeq(i).typeSymbol
        seenTypes(i) match {
          case Nil =>
            devWarning(s"base $baseClass not found in basetypes of $clazz. This might indicate incorrect caching of TypeRef#parents.")
          case _ :: Nil =>
            ;// OK
          case tp1 :: tp2 :: _ =>
            reporter.error(clazz.pos, "illegal inheritance;\n " + clazz +
                       " inherits different type instances of " + baseClass +
                       ":\n" + tp1 + " and " + tp2)
            explainTypes(tp1, tp2)
            explainTypes(tp2, tp1)
        }
      }
    }

  // Variance Checking --------------------------------------------------------

    object varianceValidator extends VarianceValidator {
      private def tpString(tp: Type) = tp match {
        case ClassInfoType(parents, _, clazz) => "supertype "+intersectionType(parents, clazz.owner)
        case _                                => "type "+tp
      }
      override def issueVarianceError(base: Symbol, sym: Symbol, required: Variance) {
        reporter.error(base.pos,
          s"${sym.variance} $sym occurs in $required position in ${tpString(base.info)} of $base")
      }
    }

// Forward reference checking ---------------------------------------------------

    class LevelInfo(val outer: LevelInfo) {
      val scope: Scope = if (outer eq null) newScope else newNestedScope(outer.scope)
      var maxindex: Int = Int.MinValue
      var refpos: Position = _
      var refsym: Symbol = _
    }

    private var currentLevel: LevelInfo = null
    private val symIndex = perRunCaches.newMap[Symbol, Int]()

    private def pushLevel() {
      currentLevel = new LevelInfo(currentLevel)
    }

    private def popLevel() {
      currentLevel = currentLevel.outer
    }

    private def enterSyms(stats: List[Tree]) {
      var index = -1
      for (stat <- stats) {
        index = index + 1
        def enterSym(sym: Symbol) = if (sym.isLocalToBlock) {
          currentLevel.scope.enter(sym)
          symIndex(sym) = index
        }

        stat match {
          case DefDef(_, _, _, _, _, _) if stat.symbol.isLazy                 =>
            enterSym(stat.symbol)
          case ClassDef(_, _, _, _) | DefDef(_, _, _, _, _, _) | ModuleDef(_, _, _) | ValDef(_, _, _, _) =>
            //assert(stat.symbol != NoSymbol, stat);//debug
            enterSym(stat.symbol.lazyAccessorOrSelf)
          case _ =>
        }
      }
    }

    private def enterReference(pos: Position, sym: Symbol) {
      if (sym.isLocalToBlock) {
        val e = currentLevel.scope.lookupEntry(sym.name)
        if ((e ne null) && sym == e.sym) {
          var l = currentLevel
          while (l.scope != e.owner) l = l.outer
          val symindex = symIndex(sym)
          if (l.maxindex < symindex) {
            l.refpos = pos
            l.refsym = sym
            l.maxindex = symindex
          }
        }
      }
    }

// Comparison checking -------------------------------------------------------
    object normalizeAll extends TypeMap {
      def apply(tp: Type) = mapOver(tp).normalize
    }

    def checkImplicitViewOptionApply(pos: Position, fn: Tree, args: List[Tree]): Unit = if (settings.warnOptionImplicit) (fn, args) match {
      case (tap@TypeApply(fun, targs), List(view: ApplyImplicitView)) if fun.symbol == currentRun.runDefinitions.Option_apply =>
        reporter.warning(pos, s"Suspicious application of an implicit view (${view.fun}) in the argument to Option.apply.") // SI-6567
      case _ =>
    }

    private def isObjectOrAnyComparisonMethod(sym: Symbol) = sym match {
      case Object_eq | Object_ne | Object_== | Object_!= | Any_== | Any_!= => true
      case _                                                               => false
    }
    /** Check the sensibility of using the given `equals` to compare `qual` and `other`. */
    private def checkSensibleEquals(pos: Position, qual: Tree, name: Name, sym: Symbol, other: Tree) = {
      def isReferenceOp = sym == Object_eq || sym == Object_ne
      def isNew(tree: Tree) = tree match {
        case Function(_, _) | Apply(Select(New(_), nme.CONSTRUCTOR), _) => true
        case _ => false
      }
      def underlyingClass(tp: Type): Symbol = {
        val sym = tp.widen.typeSymbol
        if (sym.isAbstractType) underlyingClass(sym.info.bounds.hi)
        else sym
      }
      val actual   = underlyingClass(other.tpe)
      val receiver = underlyingClass(qual.tpe)
      def onTrees[T](f: List[Tree] => T) = f(List(qual, other))
      def onSyms[T](f: List[Symbol] => T) = f(List(receiver, actual))

      // @MAT normalize for consistency in error message, otherwise only part is normalized due to use of `typeSymbol`
      def typesString = normalizeAll(qual.tpe.widen)+" and "+normalizeAll(other.tpe.widen)

      /* Symbols which limit the warnings we can issue since they may be value types */
      val isMaybeValue = Set[Symbol](AnyClass, AnyRefClass, AnyValClass, ObjectClass, ComparableClass, JavaSerializableClass)

      // Whether def equals(other: Any) has known behavior: it is the default
      // inherited from java.lang.Object, or it is a synthetically generated
      // case equals.  TODO - more cases are warnable if the target is a synthetic
      // equals.
      def isUsingWarnableEquals = {
        val m = receiver.info.member(nme.equals_)
        ((m == Object_equals) || (m == Any_equals) || isMethodCaseEquals(m))
      }
      def isMethodCaseEquals(m: Symbol) = m.isSynthetic && m.owner.isCase
      def isCaseEquals = isMethodCaseEquals(receiver.info.member(nme.equals_))
      // Whether this == or != is one of those defined in Any/AnyRef or an overload from elsewhere.
      def isUsingDefaultScalaOp = sym == Object_== || sym == Object_!= || sym == Any_== || sym == Any_!=
      def haveSubclassRelationship = (actual isSubClass receiver) || (receiver isSubClass actual)

      // Whether the operands+operator represent a warnable combo (assuming anyrefs)
      // Looking for comparisons performed with ==/!= in combination with either an
      // equals method inherited from Object or a case class synthetic equals (for
      // which we know the logic.)
      def isWarnable           = isReferenceOp || (isUsingDefaultScalaOp && isUsingWarnableEquals)
      def isEitherNullable     = (NullTpe <:< receiver.info) || (NullTpe <:< actual.info)
      def isEitherValueClass   = actual.isDerivedValueClass || receiver.isDerivedValueClass
      def isBoolean(s: Symbol) = unboxedValueClass(s) == BooleanClass
      def isUnit(s: Symbol)    = unboxedValueClass(s) == UnitClass
      def isNumeric(s: Symbol) = isNumericValueClass(unboxedValueClass(s)) || isAnyNumber(s)
      def isScalaNumber(s: Symbol) = s isSubClass ScalaNumberClass
      def isJavaNumber(s: Symbol)  = s isSubClass JavaNumberClass
      // includes java.lang.Number if appropriate [SI-5779]
      def isAnyNumber(s: Symbol)     = isScalaNumber(s) || isJavaNumber(s)
      def isMaybeAnyValue(s: Symbol) = isPrimitiveValueClass(unboxedValueClass(s)) || isMaybeValue(s)
      // used to short-circuit unrelatedTypes check if both sides are special
      def isSpecial(s: Symbol) = isMaybeAnyValue(s) || isAnyNumber(s)
      val nullCount            = onSyms(_ filter (_ == NullClass) size)
      def isNonsenseValueClassCompare = (
           !haveSubclassRelationship
        && isUsingDefaultScalaOp
        && isEitherValueClass
        && !isCaseEquals
      )

      // Have we already determined that the comparison is non-sensible? I mean, non-sensical?
      var isNonSensible = false

      def nonSensibleWarning(what: String, alwaysEqual: Boolean) = {
        val msg = alwaysEqual == (name == nme.EQ || name == nme.eq)
        reporter.warning(pos, s"comparing $what using `${name.decode}' will always yield $msg")
        isNonSensible = true
      }
      def nonSensible(pre: String, alwaysEqual: Boolean) =
        nonSensibleWarning(s"${pre}values of types $typesString", alwaysEqual)
      def nonSensiblyEq() = nonSensible("", alwaysEqual = true)
      def nonSensiblyNeq() = nonSensible("", alwaysEqual = false)
      def nonSensiblyNew() = nonSensibleWarning("a fresh object", alwaysEqual = false)

      def unrelatedMsg = name match {
        case nme.EQ | nme.eq => "never compare equal"
        case _               => "always compare unequal"
      }
      def unrelatedTypes() = if (!isNonSensible) {
        val weaselWord = if (isEitherValueClass) "" else " most likely"
        reporter.warning(pos, s"$typesString are unrelated: they will$weaselWord $unrelatedMsg")
      }

      if (nullCount == 2) // null == null
        nonSensiblyEq()
      else if (nullCount == 1) {
        if (onSyms(_ exists isPrimitiveValueClass)) // null == 5
          nonSensiblyNeq()
        else if (onTrees( _ exists isNew)) // null == new AnyRef
          nonSensiblyNew()
      }
      else if (isBoolean(receiver)) {
        if (!isBoolean(actual) && !isMaybeValue(actual))    // true == 5
          nonSensiblyNeq()
      }
      else if (isUnit(receiver)) {
        if (isUnit(actual)) // () == ()
          nonSensiblyEq()
        else if (!isUnit(actual) && !isMaybeValue(actual))  // () == "abc"
          nonSensiblyNeq()
      }
      else if (isNumeric(receiver)) {
        if (!isNumeric(actual))
          if (isUnit(actual) || isBoolean(actual) || !isMaybeValue(actual))   // 5 == "abc"
            nonSensiblyNeq()
      }
      else if (isWarnable && !isCaseEquals) {
        if (isNew(qual)) // new X == y
          nonSensiblyNew()
        else if (isNew(other) && (receiver.isEffectivelyFinal || isReferenceOp))   // object X ; X == new Y
          nonSensiblyNew()
        else if (receiver.isEffectivelyFinal && !(receiver isSubClass actual) && !actual.isRefinementClass) {  // object X, Y; X == Y
          if (isEitherNullable)
            nonSensible("non-null ", false)
          else
            nonSensiblyNeq()
        }
      }

      // warn if one but not the other is a derived value class
      // this is especially important to enable transitioning from
      // regular to value classes without silent failures.
      if (isNonsenseValueClassCompare)
        unrelatedTypes()
      // possibleNumericCount is insufficient or this will warn on e.g. Boolean == j.l.Boolean
      else if (isWarnable && nullCount == 0 && !(isSpecial(receiver) && isSpecial(actual))) {
        // better to have lubbed and lost
        def warnIfLubless(): Unit = {
          val common = global.lub(List(actual.tpe, receiver.tpe))
          if (ObjectTpe <:< common && !(ObjectTpe <:< actual.tpe && ObjectTpe <:< receiver.tpe))
            unrelatedTypes()
        }
        // warn if actual has a case parent that is not same as receiver's;
        // if actual is not a case, then warn if no common supertype, as below
        if (isCaseEquals) {
          def thisCase = receiver.info.member(nme.equals_).owner
          actual.info.baseClasses.find(_.isCase) match {
            case Some(p) if p != thisCase => nonSensible("case class ", false)
            case None =>
              // stronger message on (Some(1) == None)
              //if (receiver.isCase && receiver.isEffectivelyFinal && !(receiver isSubClass actual)) nonSensiblyNeq()
              //else
              // if a class, it must be super to thisCase (and receiver) since not <: thisCase
              if (!actual.isTrait && !(receiver isSubClass actual)) nonSensiblyNeq()
              else if (!haveSubclassRelationship) warnIfLubless()
            case _ =>
          }
        }
        // warn only if they have no common supertype below Object
        else if (!haveSubclassRelationship) {
          warnIfLubless()
        }
      }
    }
    /** Sensibility check examines flavors of equals. */
    def checkSensible(pos: Position, fn: Tree, args: List[Tree]) = fn match {
      case Select(qual, name @ (nme.EQ | nme.NE | nme.eq | nme.ne)) if args.length == 1 && isObjectOrAnyComparisonMethod(fn.symbol) && !currentOwner.isSynthetic =>
        checkSensibleEquals(pos, qual, name, fn.symbol, args.head)
      case _ =>
    }

    // SI-6276 warn for `def foo = foo` or `val bar: X = bar`, which come up more frequently than you might think.
    def checkInfiniteLoop(valOrDef: ValOrDefDef) {
      def callsSelf = valOrDef.rhs match {
        case t @ (Ident(_) | Select(This(_), _)) =>
          t hasSymbolWhich (_.accessedOrSelf == valOrDef.symbol)
        case _ => false
      }
      val trivialInfiniteLoop = (
        !valOrDef.isErroneous
     && !valOrDef.symbol.isValueParameter
     && valOrDef.symbol.paramss.isEmpty
     && callsSelf
      )
      if (trivialInfiniteLoop)
        reporter.warning(valOrDef.rhs.pos, s"${valOrDef.symbol.fullLocationString} does nothing other than call itself recursively")
    }

// Transformation ------------------------------------------------------------

    /* Convert a reference to a case factory of type `tpe` to a new of the class it produces. */
    def toConstructor(pos: Position, tpe: Type): Tree = {
      val rtpe = tpe.finalResultType
      assert(rtpe.typeSymbol hasFlag CASE, tpe)
      localTyper.typedOperator {
        atPos(pos) {
          Select(New(TypeTree(rtpe)), rtpe.typeSymbol.primaryConstructor)
        }
      }
    }

    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      pushLevel()
      try {
        enterSyms(stats)
        var index = -1
        stats flatMap { stat => index += 1; transformStat(stat, index) }
      }
      finally popLevel()
    }

    /** Eliminate ModuleDefs. In all cases the ModuleDef (carrying a module symbol) is
     *  replaced with a ClassDef (carrying the corresponding module class symbol) with additional
     *  trees created as follows:
     *
     *  1) A statically reachable object (either top-level or nested only in objects) receives
     *     no additional trees.
     *  2) An inner object which matches an existing member (e.g. implements an interface)
     *     receives an accessor DefDef to implement the interface.
     *  3) An inner object otherwise receives a private ValDef which declares a module var
     *     (the field which holds the module class - it has a name like Foo$module) and an
     *     accessor for that field. The instance is created lazily, on first access.
     */
    private def eliminateModuleDefs(moduleDef: Tree): List[Tree] = exitingRefchecks {
      val ModuleDef(_, _, impl) = moduleDef
      val module        = moduleDef.symbol
      val moduleClass   = module.moduleClass
      val site          = module.owner
      val moduleName    = module.name.toTermName
      // The typer doesn't take kindly to seeing this ClassDef; we have to
      // set NoType so it will be ignored.
      val cdef          = ClassDef(moduleClass, impl) setType NoType

      // This code is related to the fix of SI-9375, which stops adding `readResolve` methods to
      // non-static (nested) modules. Before the fix, the method would cause the module accessor
      // to become notPrivate. To prevent binary changes in the 2.11.x branch, we mimic that behavior.
      // There is a bit of code duplication between here and SyntheticMethods. We cannot call
      // makeNotPrivate already in SyntheticMethod: that is during type checking, and not all references
      // are resolved yet, so we cannot rename a definition. This code doesn't exist in the 2.12.x branch.
      def hasConcreteImpl(name: Name) = moduleClass.info.member(name).alternatives exists (m => !m.isDeferred)
      val hadReadResolveBeforeSI9375 = moduleClass.isSerializable && !hasConcreteImpl(nme.readResolve)
      if (hadReadResolveBeforeSI9375)
        moduleClass.sourceModule.makeNotPrivate(moduleClass.sourceModule.owner)

      // Create the module var unless the immediate owner is a class and
      // the module var already exists there. See SI-5012, SI-6712.
      def findOrCreateModuleVar() = {
        val vsym = (
          if (site.isTerm) NoSymbol
          else site.info decl nme.moduleVarName(moduleName)
        )
        vsym orElse (site newModuleVarSymbol module)
      }
      def newInnerObject() = {
        // Create the module var unless it is already in the module owner's scope.
        // The lookup is on module.enclClass and not module.owner lest there be a
        // nullary method between us and the class; see SI-5012.
        val moduleVar = findOrCreateModuleVar()
        val rhs       = gen.newModule(module, moduleVar.tpe)
        val body      = if (site.isTrait) rhs else gen.mkAssignAndReturn(moduleVar, rhs)
        val accessor  = DefDef(module, body.changeOwner(moduleVar -> module))

        ValDef(moduleVar) :: accessor :: Nil
      }
      def matchingInnerObject() = {
        val newFlags = (module.flags | STABLE) & ~MODULE
        val newInfo  = NullaryMethodType(moduleClass.tpe)
        val accessor = site.newMethod(moduleName, module.pos, newFlags) setInfoAndEnter newInfo

        DefDef(accessor, Select(This(site), module)) :: Nil
      }
      val newTrees = cdef :: (
        if (module.isStatic)
          if (module.isOverridingSymbol) matchingInnerObject() else Nil
        else
          newInnerObject()
      )
      transformTrees(newTrees map localTyper.typedPos(moduleDef.pos))
    }

    def transformStat(tree: Tree, index: Int): List[Tree] = tree match {
      case t if treeInfo.isSelfConstrCall(t) =>
        assert(index == 0, index)
        try transform(tree) :: Nil
        finally if (currentLevel.maxindex > 0) {
          // An implementation restriction to avoid VerifyErrors and lazyvals mishaps; see SI-4717
          debuglog("refsym = " + currentLevel.refsym)
          reporter.error(currentLevel.refpos, "forward reference not allowed from self constructor invocation")
        }
      case ModuleDef(_, _, _) => eliminateModuleDefs(tree)
      case ValDef(_, _, _, _) =>
        val tree1 = transform(tree) // important to do before forward reference check
        if (tree1.symbol.isLazy) tree1 :: Nil
        else {
          val lazySym = tree.symbol.lazyAccessorOrSelf
          if (lazySym.isLocalToBlock && index <= currentLevel.maxindex) {
            debuglog("refsym = " + currentLevel.refsym)
            reporter.error(currentLevel.refpos, "forward reference extends over definition of " + lazySym)
          }
          tree1 :: Nil
        }
      case Import(_, _)                                                                       => Nil
      case DefDef(mods, _, _, _, _, _) if (mods hasFlag MACRO) || (tree.symbol hasFlag MACRO) => Nil
      case _                                                                                  => transform(tree) :: Nil
    }

    /* Check whether argument types conform to bounds of type parameters */
    private def checkBounds(tree0: Tree, pre: Type, owner: Symbol, tparams: List[Symbol], argtps: List[Type]): Unit =
      try typer.infer.checkBounds(tree0, pre, owner, tparams, argtps, "")
      catch {
        case ex: TypeError =>
          reporter.error(tree0.pos, ex.getMessage())
          if (settings.explaintypes) {
            val bounds = tparams map (tp => tp.info.instantiateTypeParams(tparams, argtps).bounds)
            (argtps, bounds).zipped map ((targ, bound) => explainTypes(bound.lo, targ))
            (argtps, bounds).zipped map ((targ, bound) => explainTypes(targ, bound.hi))
            ()
          }
      }
    private def isIrrefutable(pat: Tree, seltpe: Type): Boolean = pat match {
      case Apply(_, args) =>
        val clazz = pat.tpe.typeSymbol
        clazz == seltpe.typeSymbol &&
        clazz.isCaseClass &&
        (args corresponds clazz.primaryConstructor.tpe.asSeenFrom(seltpe, clazz).paramTypes)(isIrrefutable)
      case Typed(pat, tpt) =>
        seltpe <:< tpt.tpe
      case Ident(tpnme.WILDCARD) =>
        true
      case Bind(_, pat) =>
        isIrrefutable(pat, seltpe)
      case _ =>
        false
    }

    // Note: if a symbol has both @deprecated and @migration annotations and both
    // warnings are enabled, only the first one checked here will be emitted.
    // I assume that's a consequence of some code trying to avoid noise by suppressing
    // warnings after the first, but I think it'd be better if we didn't have to
    // arbitrarily choose one as more important than the other.
    private def checkUndesiredProperties(sym: Symbol, pos: Position) {
      // If symbol is deprecated, and the point of reference is not enclosed
      // in either a deprecated member or a scala bridge method, issue a warning.
      // TODO: x.hasBridgeAnnotation doesn't seem to be needed here...
      if (sym.isDeprecated && !currentOwner.ownerChain.exists(x => x.isDeprecated || x.hasBridgeAnnotation))
        currentRun.reporting.deprecationWarning(pos, sym)

      // Similar to deprecation: check if the symbol is marked with @migration
      // indicating it has changed semantics between versions.
      if (sym.hasMigrationAnnotation && settings.Xmigration.value != NoScalaVersion) {
        val changed = try
          settings.Xmigration.value < ScalaVersion(sym.migrationVersion.get)
        catch {
          case e : NumberFormatException =>
            reporter.warning(pos, s"${sym.fullLocationString} has an unparsable version number: ${e.getMessage()}")
            // if we can't parse the format on the migration annotation just conservatively assume it changed
            true
        }
        if (changed)
          reporter.warning(pos, s"${sym.fullLocationString} has changed semantics in version ${sym.migrationVersion.get}:\n${sym.migrationMessage.get}")
      }
      // See an explanation of compileTimeOnly in its scaladoc at scala.annotation.compileTimeOnly.
      if (sym.isCompileTimeOnly && !currentOwner.ownerChain.exists(x => x.isCompileTimeOnly)) {
        def defaultMsg =
          sm"""Reference to ${sym.fullLocationString} should not have survived past type checking,
              |it should have been processed and eliminated during expansion of an enclosing macro."""
        // The getOrElse part should never happen, it's just here as a backstop.
        reporter.error(pos, sym.compileTimeOnlyMessage getOrElse defaultMsg)
      }
    }

    private def checkDelayedInitSelect(qual: Tree, sym: Symbol, pos: Position) = {
      def isLikelyUninitialized = (
           (sym.owner isSubClass DelayedInitClass)
        && !qual.tpe.isInstanceOf[ThisType]
        && sym.accessedOrSelf.isVal
      )
      if (settings.warnDelayedInit && isLikelyUninitialized)
        reporter.warning(pos, s"Selecting ${sym} from ${sym.owner}, which extends scala.DelayedInit, is likely to yield an uninitialized value")
    }

    private def lessAccessible(otherSym: Symbol, memberSym: Symbol): Boolean = (
         (otherSym != NoSymbol)
      && !otherSym.isProtected
      && !otherSym.isTypeParameterOrSkolem
      && !otherSym.isExistentiallyBound
      && (otherSym isLessAccessibleThan memberSym)
      && (otherSym isLessAccessibleThan memberSym.enclClass)
    )
    private def lessAccessibleSymsInType(other: Type, memberSym: Symbol): List[Symbol] = {
      val extras = other match {
        case TypeRef(pre, _, args) =>
          // checking the prefix here gives us spurious errors on e.g. a private[process]
          // object which contains a type alias, which normalizes to a visible type.
          args filterNot (_ eq NoPrefix) flatMap (tp => lessAccessibleSymsInType(tp, memberSym))
        case _ =>
          Nil
      }
      if (lessAccessible(other.typeSymbol, memberSym)) other.typeSymbol :: extras
      else extras
    }
    private def warnLessAccessible(otherSym: Symbol, memberSym: Symbol) {
      val comparison = accessFlagsToString(memberSym) match {
        case ""   => ""
        case acc  => " is " + acc + " but"
      }
      val cannot =
        if (memberSym.isDeferred) "may be unable to provide a concrete implementation of"
        else "may be unable to override"

      reporter.warning(memberSym.pos,
        "%s%s references %s %s.".format(
          memberSym.fullLocationString, comparison,
          accessFlagsToString(otherSym), otherSym
        ) + "\nClasses which cannot access %s %s %s.".format(
          otherSym.decodedName, cannot, memberSym.decodedName)
      )
    }

    /** Warn about situations where a method signature will include a type which
     *  has more restrictive access than the method itself.
     */
    private def checkAccessibilityOfReferencedTypes(tree: Tree) {
      val member = tree.symbol

      def checkAccessibilityOfType(tpe: Type) {
        val inaccessible = lessAccessibleSymsInType(tpe, member)
        // if the unnormalized type is accessible, that's good enough
        if (inaccessible.isEmpty) ()
        // or if the normalized type is, that's good too
        else if ((tpe ne tpe.normalize) && lessAccessibleSymsInType(tpe.dealiasWiden, member).isEmpty) ()
        // otherwise warn about the inaccessible syms in the unnormalized type
        else inaccessible foreach (sym => warnLessAccessible(sym, member))
      }

      // types of the value parameters
      mapParamss(member)(p => checkAccessibilityOfType(p.tpe))
      // upper bounds of type parameters
      member.typeParams.map(_.info.bounds.hi.widen) foreach checkAccessibilityOfType
    }

    private def checkByNameRightAssociativeDef(tree: DefDef) {
      tree match {
        case DefDef(_, name, _, params :: _, _, _) =>
          if (settings.warnByNameRightAssociative && !treeInfo.isLeftAssoc(name.decodedName) && params.exists(p => isByName(p.symbol)))
            reporter.warning(tree.pos,
              "by-name parameters will be evaluated eagerly when called as a right-associative infix operator. For more details, see SI-1980.")
        case _ =>
      }
    }

    /** Check that a deprecated val or def does not override a
      * concrete, non-deprecated method.  If it does, then
      * deprecation is meaningless.
      */
    private def checkDeprecatedOvers(tree: Tree) {
      val symbol = tree.symbol
      if (symbol.isDeprecated) {
        val concrOvers =
          symbol.allOverriddenSymbols.filter(sym =>
            !sym.isDeprecated && !sym.isDeferred && !sym.hasDeprecatedOverridingAnnotation && !sym.enclClass.hasDeprecatedInheritanceAnnotation)
        if(!concrOvers.isEmpty)
          currentRun.reporting.deprecationWarning(
            tree.pos,
            symbol,
            s"${symbol.toString} overrides concrete, non-deprecated symbol(s):    ${concrOvers.map(_.name.decode).mkString(", ")}")
      }
    }
    private def isRepeatedParamArg(tree: Tree) = currentApplication match {
      case Apply(fn, args) =>
        (    args.nonEmpty
          && (args.last eq tree)
          && (fn.tpe.params.length == args.length)
          && isRepeatedParamType(fn.tpe.params.last.tpe)
        )
      case _ =>
        false
    }

    private def checkTypeRef(tp: Type, tree: Tree, skipBounds: Boolean) = tp match {
      case TypeRef(pre, sym, args) =>
        tree match {
          case tt: TypeTree if tt.original == null => // SI-7783 don't warn about inferred types
                                                      // FIXME: reconcile this check with one in resetAttrs
          case _ => checkUndesiredProperties(sym, tree.pos)
        }
        if(sym.isJavaDefined)
          sym.typeParams foreach (_.cookJavaRawInfo())
        if (!tp.isHigherKinded && !skipBounds)
          checkBounds(tree, pre, sym.owner, sym.typeParams, args)
      case _ =>
    }

    private def checkTypeRefBounds(tp: Type, tree: Tree) = {
      var skipBounds = false
      tp match {
        case AnnotatedType(ann :: Nil, underlying) if ann.symbol == UncheckedBoundsClass =>
          skipBounds = true
          underlying
        case TypeRef(pre, sym, args) =>
          if (!tp.isHigherKinded && !skipBounds)
            checkBounds(tree, pre, sym.owner, sym.typeParams, args)
          tp
        case _ =>
          tp
      }
    }

    private def checkAnnotations(tpes: List[Type], tree: Tree) = tpes foreach { tp =>
      checkTypeRef(tp, tree, skipBounds = false)
      checkTypeRefBounds(tp, tree)
    }
    private def doTypeTraversal(tree: Tree)(f: Type => Unit) = if (!inPattern) tree.tpe foreach f

    private def applyRefchecksToAnnotations(tree: Tree): Unit = {
      def applyChecks(annots: List[AnnotationInfo]) = {
        checkAnnotations(annots map (_.atp), tree)
        transformTrees(annots flatMap (_.args))
      }

      tree match {
        case m: MemberDef =>
          val sym = m.symbol
          applyChecks(sym.annotations)
          // validate implicitNotFoundMessage
          analyzer.ImplicitNotFoundMsg.check(sym) foreach { warn =>
            reporter.warning(tree.pos, f"Invalid implicitNotFound message for ${sym}%s${sym.locationString}%s:%n$warn")
          }

        case tpt@TypeTree() =>
          if(tpt.original != null) {
            tpt.original foreach {
              case dc@TypeTreeWithDeferredRefCheck() =>
                applyRefchecksToAnnotations(dc.check()) // #2416
              case _ =>
            }
          }

          doTypeTraversal(tree) {
            case tp @ AnnotatedType(annots, _)  =>
              applyChecks(annots)
            case tp =>
          }
        case _ =>
      }
    }

    private def isSimpleCaseApply(tree: Tree): Boolean = {
      val sym = tree.symbol
      def isClassTypeAccessible(tree: Tree): Boolean = tree match {
        case TypeApply(fun, targs) =>
          isClassTypeAccessible(fun)
        case Select(module, apply) =>
          ( // SI-4859 `CaseClass1().InnerCaseClass2()` must not be rewritten to `new InnerCaseClass2()`;
            //          {expr; Outer}.Inner() must not be rewritten to `new Outer.Inner()`.
            treeInfo.isQualifierSafeToElide(module) &&
              // SI-5626 Classes in refinement types cannot be constructed with `new`. In this case,
              // the companion class is actually not a ClassSymbol, but a reference to an abstract type.
              module.symbol.companionClass.isClass
            )
      }

      sym.isSourceMethod &&
        sym.isCase &&
        sym.name == nme.apply &&
        isClassTypeAccessible(tree) &&
        !tree.tpe.finalResultType.typeSymbol.primaryConstructor.isLessAccessibleThan(tree.symbol)
    }

    private def transformCaseApply(tree: Tree) = {
      tree foreach {
        case i@Ident(_) =>
          enterReference(i.pos, i.symbol) // SI-5390 need to `enterReference` for `a` in `a.B()`
        case _ =>
      }
      toConstructor(tree.pos, tree.tpe)
    }

    private def transformApply(tree: Apply): Tree = tree match {
      case Apply(
        Select(qual, nme.filter | nme.withFilter),
        List(Function(
          List(ValDef(_, pname, tpt, _)),
          Match(_, CaseDef(pat1, _, _) :: _))))
        if ((pname startsWith nme.CHECK_IF_REFUTABLE_STRING) &&
            isIrrefutable(pat1, tpt.tpe) && (qual.tpe <:< tree.tpe)) =>

          transform(qual)

      case Apply(fn, args) =>
        // sensicality should be subsumed by the unreachability/exhaustivity/irrefutability
        // analyses in the pattern matcher
        if (!inPattern) {
          checkImplicitViewOptionApply(tree.pos, fn, args)
          checkSensible(tree.pos, fn, args)
        }
        currentApplication = tree
        tree
    }
    private def transformSelect(tree: Select): Tree = {
      val Select(qual, _) = tree
      val sym = tree.symbol

      checkUndesiredProperties(sym, tree.pos)
      checkDelayedInitSelect(qual, sym, tree.pos)

      if (!sym.exists)
        devWarning("Select node has NoSymbol! " + tree + " / " + tree.tpe)
      else if (sym.isLocalToThis)
        varianceValidator.checkForEscape(sym, currentClass)

      def checkSuper(mix: Name) =
        // term should have been eliminated by super accessors
        assert(!(qual.symbol.isTrait && sym.isTerm && mix == tpnme.EMPTY), (qual.symbol, sym, mix))

      // Rewrite eligible calls to monomorphic case companion apply methods to the equivalent constructor call.
      //
      // Note: for generic case classes the rewrite needs to be handled at the enclosing `TypeApply` to transform
      // `TypeApply(Select(C, apply), targs)` to `Select(New(C[targs]), <init>)`. In case such a `TypeApply`
      // was deemed ineligible for transformation (e.g. the case constructor was private), the refchecks transform
      // will recurse to this point with `Select(C, apply)`, which will have a type `[T](...)C[T]`.
      //
      // We don't need to perform the check on the Select node, and `!isHigherKinded will guard against this
      // redundant (and previously buggy, SI-9546) consideration.
      if (!tree.tpe.isHigherKinded && isSimpleCaseApply(tree)) {
        transformCaseApply(tree)
      } else {
        qual match {
          case Super(_, mix)  => checkSuper(mix)
          case _              =>
        }
        tree
      }
    }
    private def transformIf(tree: If): Tree = {
      val If(cond, thenpart, elsepart) = tree
      def unitIfEmpty(t: Tree): Tree =
        if (t == EmptyTree) Literal(Constant(())).setPos(tree.pos).setType(UnitTpe) else t

      cond.tpe match {
        case ConstantType(value) =>
          val res = if (value.booleanValue) thenpart else elsepart
          unitIfEmpty(res)
        case _ => tree
      }
    }

    // Warning about nullary methods returning Unit.
    private def checkNullaryMethodReturnType(sym: Symbol) = sym.tpe match {
      case NullaryMethodType(restpe) if restpe.typeSymbol == UnitClass =>
        // this may be the implementation of e.g. a generic method being parameterized
        // on Unit, in which case we had better let it slide.
        val isOk = (
             sym.isGetter
          || (sym.name containsName nme.DEFAULT_GETTER_STRING)
          || sym.allOverriddenSymbols.exists(over => !(over.tpe.resultType =:= sym.tpe.resultType))
        )
        if (!isOk)
          reporter.warning(sym.pos, s"side-effecting nullary methods are discouraged: suggest defining as `def ${sym.name.decode}()` instead")
      case _ => ()
    }

    // Verify classes extending AnyVal meet the requirements
    private def checkAnyValSubclass(clazz: Symbol) = {
      if (clazz.isDerivedValueClass) {
        if (clazz.isTrait)
          reporter.error(clazz.pos, "Only classes (not traits) are allowed to extend AnyVal")
        else if (clazz.hasAbstractFlag)
          reporter.error(clazz.pos, "`abstract' modifier cannot be used with value classes")
      }
    }

    private def checkUnexpandedMacro(t: Tree) =
      if (!t.isDef && t.hasSymbolField && t.symbol.isTermMacro)
        reporter.error(t.pos, "macro has not been expanded")

    override def transform(tree: Tree): Tree = {
      val savedLocalTyper = localTyper
      val savedCurrentApplication = currentApplication
      try {
        val sym = tree.symbol

        // Apply RefChecks to annotations. Makes sure the annotations conform to
        // type bounds (bug #935), issues deprecation warnings for symbols used
        // inside annotations.
        applyRefchecksToAnnotations(tree)
        var result: Tree = tree match {
          case DefDef(_, _, _, _, _, EmptyTree) if sym hasAnnotation NativeAttr =>
            sym resetFlag DEFERRED
            transform(deriveDefDef(tree)(_ => typed(gen.mkSysErrorCall("native method stub"))))

          case ValDef(_, _, _, _) | DefDef(_, _, _, _, _, _) =>
            checkDeprecatedOvers(tree)
            checkInfiniteLoop(tree.asInstanceOf[ValOrDefDef])
            if (settings.warnNullaryUnit)
              checkNullaryMethodReturnType(sym)
            if (settings.warnInaccessible) {
              if (!sym.isConstructor && !sym.isEffectivelyFinalOrNotOverridden && !sym.isSynthetic)
                checkAccessibilityOfReferencedTypes(tree)
            }
            tree match {
              case dd: DefDef => checkByNameRightAssociativeDef(dd)
              case _          =>
            }
            tree

          case Template(parents, self, body) =>
            localTyper = localTyper.atOwner(tree, currentOwner)
            validateBaseTypes(currentOwner)
            checkOverloadedRestrictions(currentOwner, currentOwner)
            // SI-7870 default getters for constructors live in the companion module
            checkOverloadedRestrictions(currentOwner, currentOwner.companionModule)
            val bridges = addVarargBridges(currentOwner)
            checkAllOverrides(currentOwner)
            checkAnyValSubclass(currentOwner)
            if (currentOwner.isDerivedValueClass)
              currentOwner.primaryConstructor makeNotPrivate NoSymbol // SI-6601, must be done *after* pickler!
            if (bridges.nonEmpty) deriveTemplate(tree)(_ ::: bridges) else tree

          case dc@TypeTreeWithDeferredRefCheck() => abort("adapt should have turned dc: TypeTreeWithDeferredRefCheck into tpt: TypeTree, with tpt.original == dc")
          case tpt@TypeTree() =>
            if(tpt.original != null) {
              tpt.original foreach {
                case dc@TypeTreeWithDeferredRefCheck() =>
                  transform(dc.check()) // #2416 -- only call transform to do refchecks, but discard results
                  // tpt has the right type if the deferred checks are ok
                case _ =>
              }
            }

            val existentialParams = new ListBuffer[Symbol]
            var skipBounds = false
            // check all bounds, except those that are existential type parameters
            // or those within typed annotated with @uncheckedBounds
            doTypeTraversal(tree) {
              case tp @ ExistentialType(tparams, tpe) =>
                existentialParams ++= tparams
              case ann: AnnotatedType if ann.hasAnnotation(UncheckedBoundsClass) =>
                // SI-7694 Allow code synthetizers to disable checking of bounds for TypeTrees based on inferred LUBs
                // which might not conform to the constraints.
                skipBounds = true
              case tp: TypeRef =>
                val tpWithWildcards = deriveTypeWithWildcards(existentialParams.toList)(tp)
                checkTypeRef(tpWithWildcards, tree, skipBounds)
              case _ =>
            }
            if (skipBounds) {
              tree.setType(tree.tpe.map {
                _.filterAnnotations(_.symbol != UncheckedBoundsClass)
              })
            }

            tree

          case TypeApply(fn, args) =>
            checkBounds(tree, NoPrefix, NoSymbol, fn.tpe.typeParams, args map (_.tpe))
            if (isSimpleCaseApply(tree))
              transformCaseApply(tree)
            else
              tree

          case x @ Apply(_, _)  =>
            transformApply(x)

          case x @ If(_, _, _)  =>
            transformIf(x)

          case New(tpt) =>
            enterReference(tree.pos, tpt.tpe.typeSymbol)
            tree

          case treeInfo.WildcardStarArg(_) if !isRepeatedParamArg(tree) =>
            reporter.error(tree.pos, "no `: _*' annotation allowed here\n"+
              "(such annotations are only allowed in arguments to *-parameters)")
            tree

          case Ident(name) =>
            checkUndesiredProperties(sym, tree.pos)
            if (name != nme.WILDCARD && name != tpnme.WILDCARD_STAR) {
              assert(sym != NoSymbol, "transformCaseApply: name = " + name.debugString + " tree = " + tree + " / " + tree.getClass) //debug
              enterReference(tree.pos, sym)
            }
            tree

          case x @ Select(_, _) =>
            transformSelect(x)

          case UnApply(fun, args) =>
            transform(fun) // just make sure we enterReference for unapply symbols, note that super.transform(tree) would not transform(fun)
                           // transformTrees(args) // TODO: is this necessary? could there be forward references in the args??
                           // probably not, until we allow parameterised extractors
            tree


          case _ => tree
        }

        // skip refchecks in patterns....
        result = result match {
          case CaseDef(pat, guard, body) =>
            val pat1 = savingInPattern {
              inPattern = true
              transform(pat)
            }
            treeCopy.CaseDef(tree, pat1, transform(guard), transform(body))
          case LabelDef(_, _, _) if treeInfo.hasSynthCaseSymbol(result) =>
            savingInPattern {
              inPattern = true
              deriveLabelDef(result)(transform)
            }
          case Apply(fun, args) if fun.symbol.isLabel && treeInfo.isSynthCaseSymbol(fun.symbol) =>
            savingInPattern {
              // SI-7756 If we were in a translated pattern, we can now switch out of pattern mode, as the label apply signals
              //         that we are in the user-supplied code in the case body.
              //
              //         Relies on the translation of:
              //            (null: Any) match { case x: List[_] => x; x.reverse; case _ => }'
              //         to:
              //            <synthetic> val x2: List[_] = (x1.asInstanceOf[List[_]]: List[_]);
              //                  matchEnd4({ x2; x2.reverse}) // case body is an argument to a label apply.
              inPattern = false
              super.transform(result)
            }
          case ValDef(_, _, _, _) if treeInfo.hasSynthCaseSymbol(result) =>
            deriveValDef(result)(transform) // SI-7716 Don't refcheck the tpt of the synthetic val that holds the selector.
          case _ =>
            super.transform(result)
        }
        result match {
          case ClassDef(_, _, _, _)
             | TypeDef(_, _, _, _) =>
            if (result.symbol.isLocalToBlock || result.symbol.isTopLevel)
              varianceValidator.traverse(result)
          case tt @ TypeTree() if tt.original != null =>
            varianceValidator.traverse(tt.original) // See SI-7872
          case _ =>
        }

        checkUnexpandedMacro(result)

        result
      } catch {
        case ex: TypeError =>
          if (settings.debug) ex.printStackTrace()
          reporter.error(tree.pos, ex.getMessage())
          tree
      } finally {
        localTyper = savedLocalTyper
        currentApplication = savedCurrentApplication
      }
    }
  }
}
