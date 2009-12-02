/* NSC -- new Scala compiler
 * Copyright 2005-2009 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc
package typechecker

import symtab.Flags._
import collection.mutable.{HashSet, HashMap}
import transform.InfoTransform
import scala.tools.nsc.util.{Position, NoPosition}
import scala.collection.mutable.ListBuffer

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
abstract class RefChecks extends InfoTransform {

  import global._
  import definitions._
  import typer.{typed, typedOperator, atOwner}

  /** the following two members override abstract members in Transform */
  val phaseName: String = "refchecks"
  override def phaseNewFlags: Long = lateMETHOD

  def newTransformer(unit: CompilationUnit): RefCheckTransformer =
    new RefCheckTransformer(unit)
  override def changesBaseClasses = false

  def transformInfo(sym: Symbol, tp: Type): Type =
    if (sym.isModule && !sym.isStatic) {
      sym setFlag (lateMETHOD | STABLE)
      PolyType(List(), tp)
    } else tp

  val toJavaRepeatedParam = new TypeMap {
    def apply(tp: Type) = tp match {
      case TypeRef(pre, RepeatedParamClass, args) =>
        typeRef(pre, JavaRepeatedParamClass, args)
      case _ =>
        mapOver(tp)
    }
  }

  class RefCheckTransformer(unit: CompilationUnit) extends Transformer {

    var localTyper: analyzer.Typer = typer;
    var currentApplication: Tree = EmptyTree
    var inPattern: Boolean = false

    // only one overloaded alternative is allowed to define default arguments
    private def checkDefaultsInOverloaded(clazz: Symbol) {
      def check(members: List[Symbol]): Unit = members match {
        case x :: xs =>
          if (x.paramss.exists(_.exists(p => p.hasFlag(DEFAULTPARAM)))) {
            val others = xs.filter(alt => {
              alt.name == x.name &&
              alt.paramss.exists(_.exists(_.hasFlag(DEFAULTPARAM))) &&
              (!alt.isConstructor || alt.owner == x.owner) // constructors of different classes are allowed to have defaults
            })
            if (!others.isEmpty) {
              val all = x :: others
              val rest = if (all.exists(_.owner != clazz)) ".\nThe members with defaults are defined in "+
                         all.map(_.owner).mkString("", " and ", ".")
              unit.error(clazz.pos, "in "+ clazz +", multiple overloaded alternatives of "+ x +
                         " define default arguments"+ rest)
              }
          }
          check(xs)
        case _ => ()
      }
      check(clazz.info.members)
    }

// Override checking ------------------------------------------------------------

    def hasRepeatedParam(tp: Type): Boolean = tp match {
      case MethodType(formals, restpe) =>
        formals.nonEmpty && formals.last.tpe.typeSymbol == RepeatedParamClass ||
        hasRepeatedParam(restpe)
      case PolyType(_, restpe) =>
        hasRepeatedParam(restpe)
      case _ =>
        false
    }

    /** Add bridges for vararg methods that extend Java vararg methods
     */
    def addVarargBridges(clazz: Symbol): List[Tree] = {
      val self = clazz.thisType

      val bridges = new ListBuffer[Tree]

      def varargBridge(member: Symbol, bridgetpe: Type): Tree = {
        val bridge = member.cloneSymbolImpl(clazz)
          .setPos(clazz.pos).setFlag(member.flags | VBRIDGE)
        bridge.setInfo(bridgetpe.cloneInfo(bridge))
        clazz.info.decls enter bridge
        val List(params) = bridge.paramss
        val TypeRef(_, JavaRepeatedParamClass, List(elemtp)) = params.last.tpe
        val (initargs, List(lastarg0)) = (params map Ident) splitAt (params.length - 1)
        val lastarg = gen.wildcardStar(gen.mkWrapArray(lastarg0, elemtp))
        val body = Apply(Select(This(clazz), member), initargs ::: List(lastarg))
        localTyper.typed {
          /*util.trace("generating varargs bridge")*/(DefDef(bridge, body))
        }
      }

      // For all concrete non-private members that have a (Scala) repeated parameter:
      //   compute the corresponding method type `jtpe` with a Java repeated parameter
      //   if a method with type `jtpe` exists and that method is not a varargs bridge
      //   then create a varargs bridge of type `jtpe` that forwards to the
      //   member method with the Scala vararg type.
      for (member <- clazz.info.nonPrivateMembers) {
        if (!(member hasFlag DEFERRED) && hasRepeatedParam(member.info)) {
          val jtpe = toJavaRepeatedParam(self.memberType(member))
          val inherited = clazz.info.nonPrivateMemberAdmitting(member.name, VBRIDGE) filter (
            sym => (self.memberType(sym) matches jtpe) && !(sym hasFlag VBRIDGE)
            // this is a bit tortuous: we look for non-private members or bridges
            // if we find a bridge everything is OK. If we find another member,
            // we need to create a bridge
          )
          if (inherited.exists) {
            bridges += varargBridge(member, jtpe)
          }
        }
      }

      bridges.toList
    }

    /** 1. Check all members of class `clazz' for overriding conditions.
     *  That is for overriding member M and overridden member O:
     *
     *    1.1. M must have the same or stronger access privileges as O.
     *    1.2. O must not be final.
     *    1.3. O is deferred, or M has `override' modifier.
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
     *  2. Check that only abstract classes have deferred members
     *  3. Check that concrete classes do not have deferred definitions
     *     that are not implemented in a subclass.
     *  4. Check that every member with an `override' modifier
     *     overrides some other member.
     */
    private def checkAllOverrides(clazz: Symbol) {

      case class MixinOverrideError(member: Symbol, msg: String)

      var mixinOverrideErrors = new ListBuffer[MixinOverrideError]()

      def printMixinOverrideErrors() {
        mixinOverrideErrors.toList match {
          case List() =>
          case List(MixinOverrideError(_, msg)) =>
            unit.error(clazz.pos, msg)
          case MixinOverrideError(member, msg) :: others =>
            val others1 = others.map(_.member.name.decode).filter(member.name.decode != _).removeDuplicates
            unit.error(
              clazz.pos,
              msg+(if (others1.isEmpty) ""
                   else ";\n other members with override errors are: "+(others1 mkString ", ")))
        }
      }

      val self = clazz.thisType

      def isAbstractTypeWithoutFBound(sym: Symbol) = // (part of DEVIRTUALIZE)
        sym.isAbstractType && !isFBounded(sym)

      def isFBounded(tsym: Symbol) =
        tsym.info.baseTypeSeq exists (_ contains tsym)

      def infoString(sym: Symbol) = infoString0(sym, sym.owner != clazz)
      def infoStringWithLocation(sym: Symbol) = infoString0(sym, true)

      def infoString0(sym: Symbol, showLocation: Boolean) = {
        val sym1 = analyzer.underlying(sym)
        sym1.toString() +
        (if (showLocation)
          sym1.locationString +
          (if (sym1.isAliasType) ", which equals "+self.memberInfo(sym1)
           else if (sym1.isAbstractType) " with bounds "+self.memberInfo(sym1)
           else if (sym1.isTerm) " of type "+self.memberInfo(sym1)
           else "")
         else "")
      }

      def overridesType(tp1: Type, tp2: Type): Boolean = (tp1.normalize, tp2.normalize) match {
        case (MethodType(List(), rtp1), PolyType(List(), rtp2)) =>
          rtp1 <:< rtp2
        case (PolyType(List(), rtp1), MethodType(List(), rtp2)) =>
          rtp1 <:< rtp2
        case (TypeRef(_, sym, _),  _) if (sym.isModuleClass) =>
          overridesType(PolyType(List(), tp1), tp2)
        case _ =>
          tp1 <:< tp2
      }

      /** Check that all conditions for overriding <code>other</code> by
       *  <code>member</code> of class <code>clazz</code> are met.
       */
      def checkOverride(clazz: Symbol, member: Symbol, other: Symbol) {

        def overrideError(msg: String) {
          if (other.tpe != ErrorType && member.tpe != ErrorType) {
            val fullmsg =
              "overriding "+infoStringWithLocation(other)+";\n "+
              infoString(member)+" "+msg+
              (if ((other.owner isSubClass member.owner) && other.isDeferred && !member.isDeferred)
                ";\n (Note that "+infoStringWithLocation(other)+" is abstract,"+
               "\n  and is therefore overridden by concrete "+infoStringWithLocation(member)+")"
               else "")
            if (member.owner == clazz) unit.error(member.pos, fullmsg)
            else mixinOverrideErrors += new MixinOverrideError(member, fullmsg)
          }
        }

        def overrideTypeError() {
          if (other.tpe != ErrorType && member.tpe != ErrorType) {
            overrideError("has incompatible type")
          }
        }

        def overrideAccessError() {
          val pwString = if (other.privateWithin == NoSymbol) ""
                         else other.privateWithin.name.toString
          val otherAccess = flagsToString(other getFlag (PRIVATE | PROTECTED), pwString)
          overrideError("has weaker access privileges; it should be "+
                        (if (otherAccess == "") "public" else "at least "+otherAccess))
        }

        //Console.println(infoString(member) + " overrides " + infoString(other) + " in " + clazz);//DEBUG

        // return if we already checked this combination elsewhere
        if (member.owner != clazz) {
          if ((member.owner isSubClass other.owner) && (member.isDeferred || !other.isDeferred)) {
            //Console.println(infoString(member) + " shadows1 " + infoString(other) " in " + clazz);//DEBUG
            return;
          }
          if (clazz.info.parents exists (parent =>
            (parent.typeSymbol isSubClass other.owner) && (parent.typeSymbol isSubClass member.owner) &&
            (member.isDeferred || !other.isDeferred))) {
              //Console.println(infoString(member) + " shadows2 " + infoString(other) + " in " + clazz);//DEBUG
                return;
            }
          if (clazz.info.parents forall (parent =>
            (parent.typeSymbol isSubClass other.owner) == (parent.typeSymbol isSubClass member.owner))) {
              //Console.println(infoString(member) + " shadows " + infoString(other) + " in " + clazz);//DEBUG
              return;
            }
        }

        /** Is the intersection between given two lists of overridden symbols empty?
         */
        def intersectionIsEmpty(syms1: List[Symbol], syms2: List[Symbol]) =
          !(syms1 exists (syms2 contains))

        if (member hasFlag PRIVATE) { // (1.1)
          overrideError("has weaker access privileges; it should not be private")
        }
        val mb = member.accessBoundary(member.owner)
        val ob = other.accessBoundary(member.owner)
        if (mb != RootClass && mb != NoSymbol && // todo: change
            (ob == RootClass || ob == NoSymbol || !ob.hasTransOwner(mb) ||
             (other hasFlag PROTECTED) && !(member hasFlag PROTECTED))) {
          overrideAccessError()
        }
        else if (other.isClass || other.isModule) {
          overrideError("cannot be used here - classes and objects cannot be overridden");
        } else if (!other.isDeferred && (member.isClass || member.isModule)) {
          overrideError("cannot be used here - classes and objects can only override abstract types");
        } else if (other hasFlag FINAL) { // (1.2)
          overrideError("cannot override final member");
        } else if (!other.isDeferred && !(member hasFlag (OVERRIDE | ABSOVERRIDE | SYNTHETIC))) { // (1.3), SYNTHETIC because of DEVIRTUALIZE
          overrideError("needs `override' modifier");
        } else if ((other hasFlag ABSOVERRIDE) && other.isIncompleteIn(clazz) && !(member hasFlag ABSOVERRIDE)) {
          overrideError("needs `abstract override' modifiers")
        } else if ((member hasFlag (OVERRIDE | ABSOVERRIDE)) &&
                   (other hasFlag ACCESSOR) && other.accessed.isVariable && !other.accessed.hasFlag(LAZY)) {
          overrideError("cannot override a mutable variable")
        } else if ((member hasFlag (OVERRIDE | ABSOVERRIDE)) &&
                   !(member.owner isSubClass other.owner) &&
                   !member.isDeferred && !other.isDeferred &&
                   intersectionIsEmpty(member.allOverriddenSymbols, other.allOverriddenSymbols)) {
          overrideError("cannot override a concrete member without a third member that's overridden by both "+
                        "(this rule is designed to prevent ``accidental overrides'')")
        } else if (other.isStable && !member.isStable) { // (1.4)
          overrideError("needs to be a stable, immutable value")
        } else if (member.isValue && (member hasFlag LAZY) &&
                   other.isValue && !other.isSourceMethod && !other.isDeferred && !(other hasFlag LAZY)) {
          overrideError("cannot override a concrete non-lazy value")
        } else if (other.isValue && (other hasFlag LAZY) && !other.isSourceMethod && !other.isDeferred &&
                   member.isValue && !(member hasFlag LAZY)) {
          overrideError("must be declared lazy to override a concrete lazy value")
        } else {
          if (other.isAliasType) {
            //if (!member.typeParams.isEmpty) // (1.5)  @MAT
            //  overrideError("may not be parameterized");
            //if (!other.typeParams.isEmpty) // (1.5)   @MAT
            //  overrideError("may not override parameterized type");
            // @M: substSym
            if (!(self.memberType(member).substSym(member.typeParams, other.typeParams) =:= self.memberType(other))) // (1.6)
              overrideTypeError();
          } else if (other.isAbstractType) {
            //if (!member.typeParams.isEmpty) // (1.7)  @MAT
            //  overrideError("may not be parameterized");
            var memberTp = self.memberType(member)
            val otherTp = self.memberInfo(other)
            if (!(otherTp.bounds containsType memberTp)) { // (1.7.1)
              overrideTypeError(); // todo: do an explaintypes with bounds here
              explainTypes(_.bounds containsType _, otherTp, memberTp)
            }

            // check overriding (abstract type --> abstract type or abstract type --> concrete type member (a type alias))
            // making an abstract type member concrete is like passing a type argument
            val kindErrors = typer.infer.checkKindBounds(List(other), List(memberTp), self, member.owner) // (1.7.2)

            if(!kindErrors.isEmpty)
              unit.error(member.pos,
                "The kind of "+member.keyString+" "+member.varianceString + member.nameString+
                " does not conform to the expected kind of " + other.defString + other.locationString + "." +
                kindErrors.toList.mkString("\n", ", ", ""))

            // check a type alias's RHS corresponds to its declaration
            // this overlaps somewhat with validateVariance
            if(member.isAliasType) {
              val kindErrors = typer.infer.checkKindBounds(List(member), List(memberTp.normalize), self, member.owner)

              if(!kindErrors.isEmpty)
                unit.error(member.pos,
                  "The kind of the right-hand side "+memberTp.normalize+" of "+member.keyString+" "+
                  member.varianceString + member.nameString+ " does not conform to its expected kind."+
                  kindErrors.toList.mkString("\n", ", ", ""))
            } else if (member.isAbstractType) {
              if (memberTp.isVolatile && !otherTp.bounds.hi.isVolatile)
                overrideError("is a volatile type; cannot override a type with non-volatile upper bound")
            }
          } else if (other.isTerm) {
            other.cookJavaRawInfo() // #2454

            if (!overridesType(self.memberInfo(member), self.memberInfo(other))) { // 8
              overrideTypeError()
              explainTypes(self.memberInfo(member), self.memberInfo(other))
            }
          }
        }
      }

      val opc = new overridingPairs.Cursor(clazz)
      while (opc.hasNext) {
        //Console.println(opc.overriding/* + ":" + opc.overriding.tpe*/ + " in "+opc.overriding.fullNameString + " overrides " + opc.overridden/* + ":" + opc.overridden.tpe*/ + " in "+opc.overridden.fullNameString + "/"+ opc.overridden.hasFlag(DEFERRED));//debug
        if (!opc.overridden.isClass) checkOverride(clazz, opc.overriding, opc.overridden);

        opc.next
      }
      printMixinOverrideErrors()

      // 2. Check that only abstract classes have deferred members
      if (clazz.isClass && !clazz.isTrait) {
        def abstractClassError(mustBeMixin: Boolean, msg: String) {
          unit.error(clazz.pos,
            (if (clazz.isAnonymousClass || clazz.isModuleClass) "object creation impossible"
             else if (mustBeMixin) clazz.toString() + " needs to be a mixin"
             else clazz.toString() + " needs to be abstract") + ", since " + msg);
          clazz.setFlag(ABSTRACT)
        }

        def javaErasedOverridingSym(sym: Symbol): Symbol =
          clazz.tpe.nonPrivateMemberAdmitting(sym.name, BRIDGE).filter(other =>
            !other.isDeferred &&
            (other hasFlag JAVA) && {
              val tp1 = erasure.erasure(clazz.thisType.memberType(sym))
              val tp2 = erasure.erasure(clazz.thisType.memberType(other))
              atPhase(currentRun.erasurePhase.next)(tp1 matches tp2)
            })

        def ignoreDeferred(member: Symbol) =
          isAbstractTypeWithoutFBound(member) ||
          ((member hasFlag JAVA) && javaErasedOverridingSym(member) != NoSymbol)

        for (member <- clazz.tpe.nonPrivateMembersAdmitting(VBRIDGE))
          if (member.isDeferred && !(clazz hasFlag ABSTRACT) && !ignoreDeferred(member)) {
            abstractClassError(
              false, infoString(member) + " is not defined" + analyzer.varNotice(member))
          } else if ((member hasFlag ABSOVERRIDE) && member.isIncompleteIn(clazz)) {
            val other = member.superSymbol(clazz);
            abstractClassError(true,
              infoString(member) + " is marked `abstract' and `override'" +
              (if (other != NoSymbol)
                " and overrides incomplete superclass member " + infoString(other)
               else ""))
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
          for (decl <- bc.info.decls.iterator) {
            if (decl.isDeferred && !ignoreDeferred(decl)) {
              val impl = decl.matchingSymbol(clazz.thisType, admit = VBRIDGE)
              if (impl == NoSymbol || (decl.owner isSubClass impl.owner)) {
                abstractClassError(false, "there is a deferred declaration of "+infoString(decl)+
                                   " which is not implemented in a subclass"+analyzer.varNotice(decl))
              }
            }
          }
          val parents = bc.info.parents
          if (!parents.isEmpty && parents.head.typeSymbol.hasFlag(ABSTRACT))
            checkNoAbstractDecls(parents.head.typeSymbol)
        }
        if (!(clazz hasFlag ABSTRACT)) checkNoAbstractDecls(clazz)
      }

      /** Returns whether there is a symbol declared in class `inclazz`
       *  (which must be different from `clazz`) whose name and type
       *  seen as a member of `class.thisType` matches `member`'s.
       */
      def hasMatchingSym(inclazz: Symbol, member: Symbol): Boolean =
        inclazz != clazz && {
          val isVarargs = hasRepeatedParam(member.tpe)
          inclazz.info.nonPrivateDecl(member.name).filter { sym =>
            !sym.isTerm || {
              val symtpe = clazz.thisType.memberType(sym)
              (member.tpe matches symtpe) || isVarargs && (toJavaRepeatedParam(member.tpe) matches symtpe)
            }
          } != NoSymbol
        }

      // 4. Check that every defined member with an `override' modifier overrides some other member.
      for (member <- clazz.info.decls.toList)
        if ((member hasFlag (OVERRIDE | ABSOVERRIDE)) &&
            !(clazz.thisType.baseClasses exists (hasMatchingSym(_, member)))) {
          // for (bc <- clazz.info.baseClasses.tail) Console.println("" + bc + " has " + bc.info.decl(member.name) + ":" + bc.info.decl(member.name).tpe);//DEBUG
          unit.error(member.pos, member.toString() + " overrides nothing");
          member resetFlag OVERRIDE
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
      val seenTypes = new Array[List[Type]](clazz.info.baseTypeSeq.length)
      for (i <- 0 until seenTypes.length) seenTypes(i) = Nil

      /** validate all base types of a class in reverse linear order. */
      def register(tp: Type) {
//        if (clazz.fullNameString.endsWith("Collection.Projection"))
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
        tp.parents foreach register
      }
      register(clazz.tpe)
      for (i <- 0 until seenTypes.length) {
        val baseClass = clazz.info.baseTypeSeq(i).typeSymbol
        seenTypes(i) match {
          case List() =>
            println("??? base "+baseClass+" not found in basetypes of "+clazz)
          case List(_) =>
            ;// OK
          case tp1 :: tp2 :: _ =>
            unit.error(clazz.pos, "illegal inheritance;\n " + clazz +
                       " inherits different type instances of " + baseClass +
                       ":\n" + tp1 + " and " + tp2);
            explainTypes(tp1, tp2)
            explainTypes(tp2, tp1)
        }
      }
    }

  // Variance Checking --------------------------------------------------------

    private val ContraVariance = -1
    private val NoVariance = 0
    private val CoVariance = 1
    private val AnyVariance = 2

    private val escapedPrivateLocals = new HashSet[Symbol]

    val varianceValidator = new Traverser {

      /** Validate variance of info of symbol `base` */
      private def validateVariance(base: Symbol) {

        def varianceString(variance: Int): String =
          if (variance == 1) "covariant"
          else if (variance == -1) "contravariant"
          else "invariant";

        /** The variance of a symbol occurrence of `tvar`
         *  seen at the level of the definition of `base`.
         *  The search proceeds from `base` to the owner of `tvar`.
         *  Initially the state is covariant, but it might change along the search.
         */
        def relativeVariance(tvar: Symbol): Int = {
          val clazz = tvar.owner
          var sym = base
          var state = CoVariance
          while (sym != clazz && state != AnyVariance) {
            //Console.println("flip: " + sym + " " + sym.isParameter());//DEBUG
            // Flip occurrences of type parameters and parameters, unless
            //  - it's a constructor, or case class factory or extractor
            //  - it's a type parameter of tvar's owner.
            if ((sym hasFlag PARAM) && !sym.owner.isConstructor && !sym.owner.isCaseApplyOrUnapply &&
                !(tvar.isTypeParameterOrSkolem && sym.isTypeParameterOrSkolem &&
                  tvar.owner == sym.owner)) state = -state;
            else if (!sym.owner.isClass ||
                     sym.isTerm && ((sym.isPrivateLocal || sym.isProtectedLocal) && !(escapedPrivateLocals contains sym))) {
              // return AnyVariance if `sym` is local to a term
              // or is private[this] or protected[this]
              state = AnyVariance
            } else if (sym.isAliasType) {
              // return AnyVariance if `sym` is an alias type
              // that does not override anything. This is OK, because we always
              // expand aliases for variance checking.
              // However, if `sym` does override a type in a base class
              // we have to assume NoVariance, as there might then be
              // references to the type parameter that are not variance checked.
              state = if (sym.allOverriddenSymbols.isEmpty) AnyVariance
                      else NoVariance
            }
            sym = sym.owner
          }
          state
        }

        /** Validate that the type `tp` is variance-correct, assuming
         *  the type occurs itself at variance position given by `variance`
         */
        def validateVariance(tp: Type, variance: Int): Unit = tp match {
          case ErrorType => ;
          case WildcardType => ;
          case NoType => ;
          case NoPrefix => ;
          case ThisType(_) => ;
          case ConstantType(_) => ;
          case DeBruijnIndex(_, _) => ;
          case SingleType(pre, sym) =>
            validateVariance(pre, variance)
          case TypeRef(pre, sym, args) =>
            if (sym.isAliasType && relativeVariance(sym) == AnyVariance)
              validateVariance(tp.normalize, variance)
            else if (sym.variance != NoVariance) {
              val v = relativeVariance(sym)
              if (v != AnyVariance && sym.variance != v * variance) {
                //Console.println("relativeVariance(" + base + "," + sym + ") = " + v);//DEBUG
                def tpString(tp: Type) = tp match {
                  case ClassInfoType(parents, _, clazz) => "supertype "+intersectionType(parents, clazz.owner)
                  case _ => "type "+tp
                }
                unit.error(base.pos,
                           varianceString(sym.variance) + " " + sym +
                           " occurs in " + varianceString(v * variance) +
                           " position in " + tpString(base.info) + " of " + base);
              }
            }
            validateVariance(pre, variance)
            validateVarianceArgs(args, variance, sym.typeParams) //@M for higher-kinded typeref, args.isEmpty
            // However, these args respect variances by construction anyway
            // -- the interesting case is in type application, see checkKindBounds in Infer
          case ClassInfoType(parents, decls, symbol) =>
            validateVariances(parents, variance)
          case RefinedType(parents, decls) =>
            validateVariances(parents, variance)
            for (sym <- decls.toList)
              validateVariance(sym.info, if (sym.isAliasType) NoVariance else variance)
          case TypeBounds(lo, hi) =>
            validateVariance(lo, -variance)
            validateVariance(hi, variance)
          case MethodType(formals, result) =>
            validateVariance(result, variance)
          case PolyType(tparams, result) =>
            // type parameters will be validated separately, because they are defined explicitly.
            validateVariance(result, variance)
          case ExistentialType(tparams, result) =>
            validateVariances(tparams map (_.info), variance)
            validateVariance(result, variance)
          case AnnotatedType(annots, tp, selfsym) =>
            if (!(annots exists (_.atp.typeSymbol.isNonBottomSubClass(uncheckedVarianceClass))))
              validateVariance(tp, variance)
        }

        def validateVariances(tps: List[Type], variance: Int) {
          tps foreach (tp => validateVariance(tp, variance))
        }

        def validateVarianceArgs(tps: List[Type], variance: Int, tparams: List[Symbol]) {
          (tps zip tparams) foreach {
            case (tp, tparam) => validateVariance(tp, variance * tparam.variance)
          }
        }

        validateVariance(base.info, CoVariance)
      }

      override def traverse(tree: Tree) {
        tree match {
          case ClassDef(_, _, _, _) |
               TypeDef(_, _, _, _) =>
            validateVariance(tree.symbol)
            super.traverse(tree)
          // ModuleDefs need not be considered because they have been eliminated already
          case ValDef(_, _, _, _) =>
            validateVariance(tree.symbol)
          case DefDef(_, _, tparams, vparamss, tpt, rhs) =>
            validateVariance(tree.symbol)
            traverseTrees(tparams); traverseTreess(vparamss)
          case Template(_, _, _) =>
            super.traverse(tree)
          case _ =>
        }
      }
    }

// Forward reference checking ---------------------------------------------------

    class LevelInfo(val outer: LevelInfo) {
      val scope: Scope = if (outer eq null) new Scope else new Scope(outer.scope)
      var maxindex: Int = Int.MinValue
      var refpos: Position = _
      var refsym: Symbol = _
    }

    private var currentLevel: LevelInfo = null
    private val symIndex = new HashMap[Symbol, Int]

    private def pushLevel() {
      currentLevel = new LevelInfo(currentLevel)
    }

    private def popLevel() {
      currentLevel = currentLevel.outer
    }

    private def enterSyms(stats: List[Tree]) {
      var index = -1
      for (stat <- stats) {
        index = index + 1;
        stat match {
          case ClassDef(_, _, _, _) | DefDef(_, _, _, _, _, _) | ModuleDef(_, _, _) | ValDef(_, _, _, _) =>
            assert(stat.symbol != NoSymbol, stat);//debug
            if (stat.symbol.isLocal) {
              currentLevel.scope.enter(stat.symbol)
              symIndex(stat.symbol) = index;
            }
          case _ =>
        }
      }
    }

    private def enterReference(pos: Position, sym: Symbol) {
      if (sym.isLocal) {
        val e = currentLevel.scope.lookupEntry(sym.name)
        if ((e ne null) && sym == e.sym) {
          var l = currentLevel
          while (l.scope != e.owner) l = l.outer;
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

    def checkSensible(pos: Position, fn: Tree, args: List[Tree]) = fn match {
      case Select(qual, name) if (args.length == 1) =>
        def isNew(tree: Tree) = tree match {
          case Function(_, _)
             | Apply(Select(New(_), nme.CONSTRUCTOR), _) => true
          case _ => false
        }
        name match {
          case nme.EQ | nme.NE | nme.LT | nme.GT | nme.LE | nme.GE =>
            def underlyingClass(tp: Type): Symbol = {
              var sym = tp.widen.typeSymbol
              while (sym.isAbstractType)
                sym = sym.info.bounds.hi.widen.typeSymbol
              sym
            }
            val formal = underlyingClass(fn.tpe.paramTypes.head)
            val actual = underlyingClass(args.head.tpe)
            val receiver = underlyingClass(qual.tpe)
            def nonSensibleWarning(what: String, alwaysEqual: Boolean) =
              unit.warning(pos, "comparing "+what+" using `"+name.decode+"' will always yield "+
                           (alwaysEqual == (name == nme.EQ || name == nme.LE || name == nme.GE)))
            def nonSensible(pre: String, alwaysEqual: Boolean) =
              nonSensibleWarning(pre+"values of types "+normalizeAll(qual.tpe.widen)+" and "+normalizeAll(args.head.tpe.widen),
                                 alwaysEqual) // @MAT normalize for consistency in error message, otherwise part is normalized due to use of `typeSymbol', but the rest isn't
            def hasObjectEquals = receiver.info.member(nme.equals_) == Object_equals
            // see if it has any == methods beyond Any's and AnyRef's: ticket #2240
            def hasOverloadedEqEq = receiver.info.member(nme.EQ).alternatives.length > 2

            if (formal == UnitClass && actual == UnitClass)
              nonSensible("", true)
            else if ((receiver == BooleanClass || receiver == UnitClass) &&
                     !(receiver isSubClass actual))
              nonSensible("", false)
            else if (isNumericValueClass(receiver) &&
                     !isNumericValueClass(actual) &&
                     !(forMSIL || (actual isSubClass BoxedNumberClass)) &&
                     !(receiver isSubClass actual))
              nonSensible("", false)
            else if ((receiver hasFlag FINAL) && hasObjectEquals && !isValueClass(receiver) &&
                     !(receiver isSubClass actual) && receiver != NullClass && actual != NullClass &&
                     (name == nme.EQ || name == nme.LE))
              nonSensible("non-null ", false)
            else if ((isNew(qual) || isNew(args.head)) && hasObjectEquals &&
                     (name == nme.EQ || name == nme.NE) && !hasOverloadedEqEq)
              nonSensibleWarning("a fresh object", false)
          case _ =>
        }
      case _ =>
    }

// Transformation ------------------------------------------------------------

    /* Convert a reference to a case factory of type `tpe' to a new of the class it produces. */
    def toConstructor(pos: Position, tpe: Type): Tree = {
      var rtpe = tpe.finalResultType
      assert(rtpe.typeSymbol hasFlag CASE, tpe);
      localTyper.typedOperator {
        atPos(pos) {
          Select(New(TypeTree(rtpe)), rtpe.typeSymbol.primaryConstructor)
        }
      }
    }

    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      pushLevel()
      enterSyms(stats)
      var index = -1
      val stats1 = stats flatMap { stat => index += 1; transformStat(stat, index) }
      popLevel()
      stats1
    }

    /** Implements lazy value accessors:
     *    - for lazy values of type Unit and all lazy fields inside traits,
     *      the rhs is the initializer itself
     *    - for all other lazy values z the accessor is a block of this form:
     *      { z = <rhs>; z } where z can be an identifier or a field.
     */
    def transformStat(tree: Tree, index: Int): List[Tree] = tree match {
      case ModuleDef(mods, name, impl) =>
        val sym = tree.symbol
        val cdef = ClassDef(mods | MODULE, name, List(), impl)
          .setPos(tree.pos)
          .setSymbol(sym.moduleClass)
          .setType(NoType)
        if (sym.isStatic) {
          if (!sym.allOverriddenSymbols.isEmpty) {
            val factory = sym.owner.newMethod(sym.pos, sym.name)
              .setFlag(sym.flags | STABLE).resetFlag(MODULE)
              .setInfo(PolyType(List(), sym.moduleClass.tpe))
            sym.owner.info.decls.enter(factory)
            val ddef =
              atPhase(phase.next) {
                localTyper.typed {
                  gen.mkModuleAccessDef(factory, sym.tpe)
                }
              }
            transformTrees(List(cdef, ddef))
          } else {
            List(transform(cdef))
          }
        } else {
          val vdef = localTyper.typedPos(tree.pos) { gen.mkModuleVarDef(sym) }
          val ddef =
            atPhase(phase.next) {
              localTyper.typed {
                if (sym.owner.isTrait) gen.mkModuleAccessDcl(sym)
                else gen.mkCachedModuleAccessDef(sym, vdef.symbol)
              }
            }

          if (sym.owner.isTrait) transformTrees(List(cdef, ddef))
          else transformTrees(List(cdef, vdef, ddef))
        }

      case ValDef(_, _, _, _) =>
        val tree1 = transform(tree); // important to do before forward reference check
        val ValDef(_, _, _, rhs) = tree1
        if (tree.symbol.hasFlag(LAZY)) {
          assert(tree.symbol.isTerm, tree.symbol)
          val vsym = tree.symbol
          val hasUnitType = (tree.symbol.tpe.typeSymbol == UnitClass)
          val lazyDefSym = vsym.lazyAccessor
          assert(lazyDefSym != NoSymbol, vsym)
          val ownerTransformer = new ChangeOwnerTraverser(vsym, lazyDefSym)
          val lazyDef = atPos(tree.pos)(
              DefDef(lazyDefSym, ownerTransformer(
                if (tree.symbol.owner.isTrait // for traits, this is further tranformed in mixins
                    || hasUnitType) rhs
                else Block(List(
                       Assign(gen.mkAttributedRef(vsym), rhs)),
                       gen.mkAttributedRef(vsym)))))
          log("Made lazy def: " + lazyDef)
          if (hasUnitType)
            typed(lazyDef) :: Nil
          else
            typed(ValDef(vsym, EmptyTree)) :: typed(lazyDef) :: Nil
        } else {
          if (tree.symbol.isLocal && index <= currentLevel.maxindex && !tree.symbol.hasFlag(LAZY)) {
            if (settings.debug.value) Console.println(currentLevel.refsym);
            unit.error(currentLevel.refpos, "forward reference extends over definition of " + tree.symbol);
          }
          List(tree1)
        }

      case Import(_, _) =>
        List()

      case _ =>
        List(transform(tree))
    }

    /******** Begin transform inner function section ********/

    /** The private functions between here and 'transform' are conceptually
     *  inner functions to that method, but have been moved outside of it to
     *  ease the burden on the optimizer.
     */

    /* Check whether argument types conform to bounds of type parameters */
    private def checkBounds(pre: Type, owner: Symbol, tparams: List[Symbol], argtps: List[Type], pos: Position): Unit =
      try typer.infer.checkBounds(pos, pre, owner, tparams, argtps, "")
      catch {
        case ex: TypeError =>
          unit.error(pos, ex.getMessage());
          if (settings.explaintypes.value) {
            val bounds = tparams map (tp => tp.info.instantiateTypeParams(tparams, argtps).bounds)
            (argtps, bounds).zipped map ((targ, bound) => explainTypes(bound.lo, targ))
            (argtps, bounds).zipped map ((targ, bound) => explainTypes(targ, bound.hi))
            ()
          }
      }
    private def isIrrefutable(pat: Tree, seltpe: Type): Boolean = {
      val result = pat match {
        case Apply(_, args) =>
          val clazz = pat.tpe.typeSymbol;
          clazz == seltpe.typeSymbol &&
          clazz.isClass && (clazz hasFlag CASE) &&
          ((args, clazz.primaryConstructor.tpe.asSeenFrom(seltpe, clazz).paramTypes).zipped forall isIrrefutable)
        case Typed(pat, tpt) =>
          seltpe <:< tpt.tpe
        case Ident(nme.WILDCARD) =>
          true
        case Bind(_, pat) =>
          isIrrefutable(pat, seltpe)
        case _ =>
          false
      }
      //Console.println("is irefutable? " + pat + ":" + pat.tpe + " against " + seltpe + ": " + result);//DEBUG
      result
    }
    /** If symbol is deprecated and is not contained in a deprecated definition,
     *  issue a deprecated warning
     */
    private def checkDeprecated(sym: Symbol, pos: Position) {
      if (sym.isDeprecated && !currentOwner.ownerChain.exists(_.isDeprecated)) {
        val dmsg = sym.deprecationMessage
        val msg = sym.toString + sym.locationString +" is deprecated"+
                  (if (dmsg.isDefined) ": "+ dmsg.get
                   else "")
        unit.deprecationWarning(pos, msg)
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
            !sym.isDeprecated && !sym.isDeferred)
        if(!concrOvers.isEmpty)
          unit.deprecationWarning(
            tree.pos,
            symbol.toString + " overrides concrete, non-deprecated symbol(s):" +
            concrOvers.map(_.name.decode).mkString("    ", ", ", ""))
      }
    }
    private def isRepeatedParamArg(tree: Tree) = currentApplication match {
      case Apply(fn, args) =>
        !args.isEmpty && (args.last eq tree) &&
        fn.tpe.paramTypes.length == args.length && isRepeatedParamType(fn.tpe.paramTypes.last)
      case _ =>
        false
    }
    private def checkTypeRef(tp: Type, pos: Position) = tp match {
      case TypeRef(pre, sym, args) =>
        checkDeprecated(sym, pos)
        if(sym.hasFlag(JAVA))
          sym.typeParams foreach (_.cookJavaRawInfo())
        if (!tp.isHigherKinded)
          checkBounds(pre, sym.owner, sym.typeParams, args, pos)
      case _ =>
    }

    private def checkAnnotations(tpes: List[Type], pos: Position) = tpes foreach (tp => checkTypeRef(tp, pos))
    private def doTypeTraversal(tree: Tree)(f: Type => Unit) = if (!inPattern) tree.tpe foreach f

    private def applyRefchecksToAnnotations(tree: Tree) = tree match {
      case m: MemberDef =>
        checkAnnotations(m.symbol.annotations map (_.atp), tree.pos)
        transformTrees(m.symbol.annotations.flatMap(_.args))
      case TypeTree() => doTypeTraversal(tree) {
        case AnnotatedType(annots, _, _) =>
          checkAnnotations(annots map (_.atp), tree.pos)
          transformTrees(annots.flatMap(_.args))
        case _ =>
      }
      case _ =>
    }

    private def transformCaseApply(tree: Tree, ifNot: => Unit) = {
      val sym = tree.symbol

      if (sym.isSourceMethod && sym.hasFlag(CASE) && sym.name == nme.apply)
        toConstructor(tree.pos, tree.tpe)
      else {
        ifNot
        tree
      }
    }

    private def transformApply(tree: Apply): Tree = tree match {
      case Apply(
        Select(qual, nme.filter),
        List(Function(
          List(ValDef(_, pname, tpt, _)),
          Match(_, CaseDef(pat1, _, _) :: _))))
        if ((pname startsWith nme.CHECK_IF_REFUTABLE_STRING) &&
            isIrrefutable(pat1, tpt.tpe) && (qual.tpe <:< tree.tpe)) =>

          qual

      case Apply(Select(New(tpt), name), args)
      if (tpt.tpe.typeSymbol == ArrayClass && args.length >= 2) =>
        unit.deprecationWarning(tree.pos,
          "new Array(...) with multiple dimensions has been deprecated; use Array.ofDim(...) instead")
        val manif = {
          var etpe = tpt.tpe
          for (_ <- args) { etpe = etpe.typeArgs.headOption.getOrElse(NoType) }
          if (etpe == NoType) {
            unit.error(tree.pos, "too many dimensions for array creation")
            Literal(Constant(null))
          } else {
            localTyper.getManifestTree(tree.pos, etpe, false)
          }
        }
        val newResult = localTyper.typedPos(tree.pos) {
          Apply(Apply(Select(gen.mkAttributedRef(ArrayModule), nme.ofDim), args), List(manif))
        }
        currentApplication = tree
        newResult

      case Apply(fn, args) =>
        checkSensible(tree.pos, fn, args)
        currentApplication = tree
        tree
    }
    private def transformSelect(tree: Select): Tree = {
      val Select(qual, name) = tree
      val sym = tree.symbol
      checkDeprecated(sym, tree.pos)

      if (currentClass != sym.owner && (sym hasFlag LOCAL)) {
        var o = currentClass
        var hidden = false
        while (!hidden && o != sym.owner && o != sym.owner.moduleClass && !o.isPackage) {
          hidden = o.isTerm || o.isPrivateLocal
          o = o.owner
        }
        if (!hidden) escapedPrivateLocals += sym
      }

      def checkSuper(mix: Name) =
        // term should have been eliminated by super accessors
        assert(!(qual.symbol.isTrait && sym.isTerm && mix == nme.EMPTY.toTypeName))

      transformCaseApply(tree,
        qual match {
          case Super(_, mix)  => checkSuper(mix)
          case _              =>
        }
      )
    }
    private def transformIf(tree: If): Tree = {
      val If(cond, thenpart, elsepart) = tree
      def unitIfEmpty(t: Tree): Tree =
        if (t == EmptyTree) Literal(()).setPos(tree.pos).setType(UnitClass.tpe) else t

      cond.tpe match {
        case ConstantType(value) =>
          val res = if (value.booleanValue) thenpart else elsepart
          unitIfEmpty(res)
        case _ => tree
      }
    }

    override def transform(tree: Tree): Tree = try {
      val savedLocalTyper = localTyper
      val savedCurrentApplication = currentApplication
      val sym = tree.symbol

      // Apply RefChecks to annotations. Makes sure the annotations conform to
      // type bounds (bug #935), issues deprecation warnings for symbols used
      // inside annotations.
      applyRefchecksToAnnotations(tree)

      var result: Tree = tree match {
        case DefDef(mods, name, tparams, vparams, tpt, EmptyTree) if tree.symbol.hasAnnotation(NativeAttr) =>
          tree.symbol.resetFlag(DEFERRED)
          transform(treeCopy.DefDef(tree, mods, name, tparams, vparams, tpt,
                typed(Apply(gen.mkAttributedRef(Predef_error), List(Literal("native method stub"))))))

        case ValDef(_, _, _, _) | DefDef(_, _, _, _, _, _) =>
          checkDeprecatedOvers(tree)
          tree

        case Template(parents, self, body) =>
          localTyper = localTyper.atOwner(tree, currentOwner)
          validateBaseTypes(currentOwner)
          checkDefaultsInOverloaded(currentOwner)
          val bridges = addVarargBridges(currentOwner)
          checkAllOverrides(currentOwner)

          if (bridges.nonEmpty) treeCopy.Template(tree, parents, self, body ::: bridges)
          else tree

        case TypeTree() =>
          val existentialParams = new ListBuffer[Symbol]
          doTypeTraversal(tree) { // check all bounds, except those that are
                            // existential type parameters
            case ExistentialType(tparams, tpe) =>
              existentialParams ++= tparams
            case t: TypeRef =>
              val exparams = existentialParams.toList
              val wildcards = exparams map (_ => WildcardType)
              checkTypeRef(t.subst(exparams, wildcards), tree.pos)
            case _ =>
          }
          tree

        case TypeApply(fn, args) =>
          checkBounds(NoPrefix, NoSymbol, fn.tpe.typeParams, args map (_.tpe), tree.pos)
          transformCaseApply(tree, ())

        case x @ Apply(_, _)  =>
          transformApply(x)

        case x @ If(_, _, _)  =>
          transformIf(x)

        case New(tpt) =>
          enterReference(tree.pos, tpt.tpe.typeSymbol)
          tree

        case Typed(expr, tpt @ Ident(name)) if name == nme.WILDCARD_STAR.toTypeName && !isRepeatedParamArg(tree) =>
          unit.error(tree.pos, "no `: _*' annotation allowed here\n"+
            "(such annotations are only allowed in arguments to *-parameters)")
          tree

        case Ident(name) =>
          transformCaseApply(tree,
            if (name != nme.WILDCARD && name != nme.WILDCARD_STAR.toTypeName) {
              assert(sym != NoSymbol, tree) //debug
              enterReference(tree.pos, sym)
            }
          )

        case x @ Select(_, _) =>
          transformSelect(x)

        case _ => tree
      }
      result = result match {
        case CaseDef(pat, guard, body) =>
          inPattern = true
          val pat1 = transform(pat)
          inPattern = false
          treeCopy.CaseDef(tree, pat1, transform(guard), transform(body))
        case _ =>
          super.transform(result)
      }
      result match {
        case ClassDef(_, _, _, _)
           | TypeDef(_, _, _, _) =>
          if (result.symbol.isLocal || result.symbol.owner.isPackageClass)
            varianceValidator.traverse(result)
        case _ =>
      }
      localTyper = savedLocalTyper
      currentApplication = savedCurrentApplication
      result
    } catch {
      case ex: TypeError =>
        if (settings.debug.value) ex.printStackTrace();
        unit.error(tree.pos, ex.getMessage())
        tree
    }
  }
}
