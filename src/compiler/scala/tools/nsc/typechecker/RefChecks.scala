/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc.typechecker

import symtab.Flags._
import collection.mutable.{HashSet, HashMap}
import transform.InfoTransform
import scala.tools.nsc.util.{Position, NoPosition}

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
  import posAssigner.atPos

  /** the following two members override abstract members in Transform */
  val phaseName: String = "refchecks"
  override def phaseNewFlags: long = lateMETHOD

  def newTransformer(unit: CompilationUnit): RefCheckTransformer =
    new RefCheckTransformer(unit)
  override def changesBaseClasses = false

  def transformInfo(sym: Symbol, tp: Type): Type =
    if (sym.isModule && !sym.isStatic) {
      sym setFlag (lateMETHOD | STABLE)
      PolyType(List(), tp)
    } else tp

  class RefCheckTransformer(unit: CompilationUnit) extends Transformer {

    var localTyper: analyzer.Typer = typer;

// Override checking ------------------------------------------------------------

    /** 1. Check all members of class `clazz' for overriding conditions.
     *  That is for overriding member M and overridden member O:
     *
     *    1.1. M must have the same or stronger access privileges as O.
     *    1.2. O must not be final.
     *    1.3. O is deferred, or M has `override' modifier.
     *    1.4. If O is an immutable value, then so is M.
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
    private def checkAllOverrides(clazz: Symbol): unit = {

      val self = clazz.thisType

      def infoString(sym: Symbol) = {
        val sym1 = analyzer.underlying(sym)
        sym1.toString() +
        (if (sym1.owner == clazz) ""
         else (sym1.locationString +
               (if (sym1.isAliasType) ", which equals "+self.memberInfo(sym1)
                else if (sym1.isAbstractType) " with bounds "+self.memberInfo(sym1)
                else if (sym1.isTerm) " of type "+self.memberInfo(sym1)
                else "")))
      }

      def overridesType(tp1: Type, tp2: Type): boolean = (tp1.normalize, tp2.normalize) match {
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
       *  <code>member</code> are met.
       */
      def checkOverride(clazz: Symbol, member: Symbol, other: Symbol): unit = {
        val pos = if (member.owner == clazz) member.pos else clazz.pos

        def overrideError(msg: String): unit =
          if (other.tpe != ErrorType && member.tpe != ErrorType)
            unit.error(pos, "error overriding " + infoString(other) +
                       ";\n " + infoString(member) + " " + msg);

        def overrideTypeError(): unit = {
          if (other.tpe != ErrorType && member.tpe != ErrorType) {
            overrideError("has incompatible type "+analyzer.underlying(member).tpe.normalize);
            explainTypes(member.tpe, other.tpe);
          }
        }

        def overrideAccessError(): unit = {
          val pwString = if (other.privateWithin == NoSymbol) ""
                         else other.privateWithin.name.toString
          val otherAccess = flagsToString(other getFlag (PRIVATE | PROTECTED), pwString)
          overrideError("has weaker access privileges; it should be "+
                        (if (otherAccess == "") "public" else "at least "+otherAccess))
        }

        //Console.println(infoString(member) + " overrides " + infoString(other) + " in " + clazz);//DEBUG

        // return if we already checked this combination elsewhere
        if (member.owner != clazz) {
          if ((member.owner isSubClass other.owner) &&
              ((member hasFlag DEFERRED) || !(other hasFlag DEFERRED))) {
                //Console.println(infoString(member) + " shadows1 " + infoString(other) " in " + clazz);//DEBUG
                return;
              }
          if (clazz.info.parents exists (parent =>
            (parent.typeSymbol isSubClass other.owner) && (parent.typeSymbol isSubClass member.owner) &&
            ((member hasFlag DEFERRED) || !(other hasFlag DEFERRED)))) {
              //Console.println(infoString(member) + " shadows2 " + infoString(other) + " in " + clazz);//DEBUG
                return;
            }
          if (clazz.info.parents forall (parent =>
            (parent.typeSymbol isSubClass other.owner) == (parent.typeSymbol isSubClass member.owner))) {
              //Console.println(infoString(member) + " shadows " + infoString(other) + " in " + clazz);//DEBUG
              return;
            }
        }

        if (member hasFlag PRIVATE) { // (1.1)
          overrideError("has weaker access privileges; it should not be private")
        }
        val mb = member.accessBoundary(member.owner)
        val ob = other.accessBoundary(member.owner)
        if (mb != RootClass && mb != NoSymbol && // todo: change
            (ob == RootClass || ob == NoSymbol ||
             mb != ob && !(ob.ownerChain contains mb) ||
             (other hasFlag PROTECTED) && !(member hasFlag PROTECTED))) {
          overrideAccessError()
        } else if (other hasFlag FINAL) { // (1.2)
          overrideError("cannot override final member");
        } else if (!(other hasFlag DEFERRED) && !(member hasFlag (OVERRIDE | ABSOVERRIDE))) { // (1.3)
          overrideError("needs `override' modifier");
        } else if ((other hasFlag ABSOVERRIDE) && other.isIncompleteIn(clazz) && !(member hasFlag ABSOVERRIDE)) {
          overrideError("needs `abstract override' modifiers")
        } else if ((member hasFlag (OVERRIDE | ABSOVERRIDE)) &&
                   (other hasFlag ACCESSOR) && other.accessed.isVariable && !other.accessed.hasFlag(LAZY)) {
          overrideError("cannot override a mutable variable")
        } else if (other.isStable && !member.isStable) { // (1.4)
          overrideError("needs to be an immutable value")
        } else if (other.isStable && !(other hasFlag DEFERRED) && other.owner.isTrait && (member hasFlag OVERRIDE)) {
          overrideError("cannot override a value or variable definition in a trait " +
                        "\n (this is an implementation restriction)")
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

            if (!(self.memberInfo(other).bounds containsType memberTp)) { // (1.7.1) {
              overrideTypeError(); // todo: do an explaintypes with bounds here
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
            }
          } else if (other.isTerm) {
            if (!overridesType(self.memberInfo(member), self.memberInfo(other))) { // 8
              overrideTypeError();
            }
          }
        }
      }

      val opc = new overridingPairs.Cursor(clazz)
      while (opc.hasNext) {
        //Console.println("overrides " + opc.overriding/* + ":" + opc.overriding.tpe*/ + opc.overriding.locationString + " " + opc.overridden/* + ":" + opc.overridden.tpe*/ + opc.overridden.locationString + opc.overridden.hasFlag(DEFERRED));//DEBUG
        if (!opc.overridden.isClass) checkOverride(clazz, opc.overriding, opc.overridden);

        opc.next
      }
      // 2. Check that only abstract classes have deferred members
      if (clazz.isClass && !clazz.isTrait) {
        def abstractClassError(mustBeMixin: boolean, msg: String): unit = {
          unit.error(clazz.pos,
            (if (clazz.isAnonymousClass || clazz.isModuleClass) "object creation impossible"
             else if (mustBeMixin) clazz.toString() + " needs to be a mixin"
             else clazz.toString() + " needs to be abstract") + ", since " + msg);
          clazz.setFlag(ABSTRACT)
        }
        for (val member <- clazz.tpe.nonPrivateMembers)
          if ((member hasFlag DEFERRED) && !(clazz hasFlag ABSTRACT)) {
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
        def checkNoAbstractDecls(bc: Symbol) {
          for (val decl <- bc.info.decls.elements) {
            if (decl hasFlag DEFERRED) {
              val impl = decl.matchingSymbol(clazz.thisType)
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

      // 4. Check that every defined member with an `override' modifier overrides some other member.
      for (val member <- clazz.info.decls.toList)
        if ((member hasFlag (OVERRIDE | ABSOVERRIDE)) &&
            (clazz.info.baseClasses.tail forall {
               bc => member.matchingSymbol(bc, clazz.thisType) == NoSymbol
            })) {
          // for (val bc <- clazz.info.baseClasses.tail) Console.println("" + bc + " has " + bc.info.decl(member.name) + ":" + bc.info.decl(member.name).tpe);//DEBUG
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
     *    <li> <!-- 2 -->
     *      Check that case classes do not inherit from case classes.
     *    </li>
     *    <li> <!-- 3 -->
     *      Check that at most one base type is a case-class.
     *    </li>
     *    <li> <!-- 4 -->
     *      Check that inner classes do not inherit from Annotation
     *    </li>
     *  </ol>
     */
    private def validateBaseTypes(clazz: Symbol): unit = {
      val seenTypes = new Array[Type](clazz.info.closure.length)
      var seenCaseClass = if (clazz hasFlag CASE) clazz else NoSymbol

      def validateTypes(tps: List[Type], includeSuper: boolean): unit = {
        if (!tps.isEmpty) {
          for (val tp <- tps.tail.reverse) validateType(tp, false);
          if (includeSuper) validateType(tps.head, true);
        }
      }

      def validateType(tp: Type, includeSuper: boolean): unit = {
        val baseClass = tp.typeSymbol
        if (baseClass.isClass) {
          val index = clazz.info.closurePos(baseClass)
          if (index >= 0) {
            if ((seenTypes(index) ne null) && !(seenTypes(index) <:< tp))
              unit.error(clazz.pos, "illegal inheritance;\n " + clazz +
                         " inherits different type instances of " + baseClass +
                         ":\n" + tp + " and " + seenTypes(index));
            seenTypes(index) = tp;
            // check that case classes do not inherit from case classes
            if (baseClass hasFlag CASE) {
              if (seenCaseClass != NoSymbol && seenCaseClass != baseClass)
                unit.error(clazz.pos, "implementation restriction: case " +
                           seenCaseClass + " and case " + baseClass +
                           " cannot be combined in one object");
              seenCaseClass = baseClass
            }
            // check that inner classes do not inherit from Annotation
            if (baseClass == ClassfileAnnotationClass)
              if (!clazz.owner.isPackageClass)
                unit.error(clazz.pos, "inner classes cannot be classfile annotations")
          }
          validateTypes(tp.parents, includeSuper)
        }
      }

      validateTypes(clazz.info.parents, true)
    }

  // Variance Checking --------------------------------------------------------

    private val ContraVariance = -1
    private val NoVariance = 0
    private val CoVariance = 1
    private val AnyVariance = 2

    private val escapedPrivateLocals = new HashSet[Symbol]

    val varianceValidator = new Traverser {

      private def validateVariance(base: Symbol) {

        def varianceString(variance: int): String =
          if (variance == 1) "covariant"
          else if (variance == -1) "contravariant"
          else "invariant";

        def relativeVariance(tvar: Symbol): int = {
          val clazz = tvar.owner
          var sym = base
          var state = CoVariance
          while (sym != clazz && state != AnyVariance) {
            //Console.println("flip: " + sym + " " + sym.isParameter());//DEBUG
            if ((sym hasFlag PARAM) && !sym.owner.isConstructor && !sym.owner.isCaseFactory &&
                !(tvar.isTypeParameterOrSkolem && sym.isTypeParameterOrSkolem &&
                  tvar.owner == sym.owner)) state = -state;
            else if (!sym.owner.isClass ||
                     ((sym.isPrivateLocal || sym.isProtectedLocal) && !(escapedPrivateLocals contains sym)))
              state = AnyVariance
            else if (sym.isAliasType)
              state = NoVariance
            sym = sym.owner
          }
          state
        }

        def validateVariance(tp: Type, variance: int): unit = tp match {
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
            if (sym.variance != NoVariance) {
              val v = relativeVariance(sym);
              if (v != AnyVariance && sym.variance != v * variance) {
                //Console.println("relativeVariance(" + base + "," + sym + ") = " + v);//DEBUG
                unit.error(base.pos,
                           varianceString(sym.variance) + " " + sym +
                           " occurs in " + varianceString(v * variance) +
                           " position in type " + base.info + " of " + base);
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
          case TypeBounds(lo, hi) =>
            validateVariance(lo, -variance)
            validateVariance(hi, variance)
          case MethodType(formals, result) =>
            validateVariance(result, variance)
          case PolyType(tparams, result) =>
            validateVariance(result, variance)
          case ExistentialType(tparams, result) =>
            validateVariance(result, variance)
          case AnnotatedType(attribs, tp) =>
            validateVariance(tp, variance)
        }

        def validateVariances(tps: List[Type], variance: int): unit =
          tps foreach (tp => validateVariance(tp, variance))

        def validateVarianceArgs(tps: List[Type], variance: int, tparams: List[Symbol]): unit =
          (tps zip tparams) foreach {
            case (tp, tparam) => validateVariance(tp, variance * tparam.variance)
          }

        validateVariance(base.info, CoVariance)
      }

      override def traverse(tree: Tree): Unit = {
        tree match {
          case ClassDef(_, _, _, _) | TypeDef(_, _, _, _) =>
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
      val scope: Scope = if (outer eq null) newScope else newScope(outer.scope)
      var maxindex: int = Math.MIN_INT
      var refpos: Position = _
      var refsym: Symbol = _
    }

    private var currentLevel: LevelInfo = null
    private val symIndex = new HashMap[Symbol, int]

    private def pushLevel(): unit =
      currentLevel = new LevelInfo(currentLevel)

    private def popLevel(): unit =
      currentLevel = currentLevel.outer

    private def enterSyms(stats: List[Tree]): unit = {
      var index = -1
      for (val stat <- stats) {
        index = index + 1;
        stat match {
          case ClassDef(_, _, _, _) | DefDef(_, _, _, _, _, _) | ModuleDef(_, _, _) | ValDef(_, _, _, _) =>
            assert(stat.symbol != NoSymbol, stat);//debug
            if (stat.symbol.isLocal) {
              currentLevel.scope.enter(newScopeEntry(stat.symbol, currentLevel.scope));
              symIndex(stat.symbol) = index;
            }
          case _ =>
        }
      }
    }

    private def enterReference(pos: Position, sym: Symbol): unit =
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
            def nonSensibleWarning(what: String, alwaysEqual: boolean) =
              unit.warning(pos, "comparing "+what+" using `"+name.decode+"' will always yield "+
                           (alwaysEqual == (name == nme.EQ || name == nme.LE || name == nme.GE)))
            def nonSensible(pre: String, alwaysEqual: boolean) =
              nonSensibleWarning(pre+"values of types "+normalizeAll(qual.tpe.widen)+" and "+normalizeAll(args.head.tpe.widen),
                                 alwaysEqual) // @MAT normalize for consistency in error message, otherwise part is normalized due to use of `typeSymbol', but the rest isn't
            def hasObjectEquals = receiver.info.member(nme.equals_) == Object_equals
            if (formal == UnitClass && actual == UnitClass)
              nonSensible("", true)
            else if ((receiver == BooleanClass || receiver == UnitClass) &&
                     !(receiver isSubClass actual))
              nonSensible("", false)
            else if (isNumericValueClass(receiver) &&
                     !isNumericValueClass(actual) &&
                     !(forMSIL || forCLDC|| (actual isSubClass BoxedNumberClass)) &&
                     !(receiver isSubClass actual))
              nonSensible("", false)
            else if ((receiver hasFlag FINAL) && hasObjectEquals && !isValueClass(receiver) &&
                     !(receiver isSubClass actual) && receiver != AllRefClass && actual != AllRefClass)
              nonSensible("non-null ", false)
            else if ((isNew(qual) || isNew(args.head)) && hasObjectEquals)
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

    def isConcreteLocalCaseFactory(clazz: Symbol) =
      (clazz hasFlag CASE) && !(clazz hasFlag ABSTRACT) && !(clazz.owner hasFlag PACKAGE)

    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      pushLevel()
      enterSyms(stats)
      var index = -1
      val stats1 = stats flatMap { stat => index = index + 1; transformStat(stat, index) }
      popLevel()
      stats1
    }

    /** Implements lazy value accessors:
     *    - for lazy fields inside traits, the rhs is the initializer itself
     *    - for all other lazy values z the accessor is a block of this form:
     *      { z = <rhs>; z } where z can be an identifier or a field.
     */
    def transformStat(tree: Tree, index: int): List[Tree] = tree match {
      case ModuleDef(mods, name, impl) =>
        val sym = tree.symbol
        val cdef = ClassDef(mods | MODULE, name, List(), impl)
          .setPos(tree.pos)
          .setSymbol(sym.moduleClass)
          .setType(NoType);
        if (sym.isStatic) List(transform(cdef))
        else {
          val vdef =
            localTyper.typed {
              atPos(tree.pos) {
                gen.mkModuleVarDef(sym)
              }
            }

          val ddef =
            atPhase(phase.next) {
              localTyper.typed {
                if (sym.owner.isTrait) gen.mkModuleAccessDcl(sym)
                else gen.mkModuleAccessDef(sym, vdef.symbol)
              }
            }

          if (sym.owner.isTrait) transformTrees(List(cdef, ddef))
          else transformTrees(List(cdef, vdef, ddef))
        }

      case ClassDef(_, _, _, _) if isConcreteLocalCaseFactory(tree.symbol) =>
        val clazz = tree.symbol
        val factory = clazz.caseFactory
        if (factory == NoSymbol) {
          assert(clazz.owner.isTerm, clazz)
          List(transform(tree))
        } else {
          def mkArgument(vparam: Symbol) = {
            val id = Ident(vparam)
            if (vparam.tpe.typeSymbol == RepeatedParamClass) Typed(id, Ident(nme.WILDCARD_STAR.toTypeName))
            else id
          }
          val caseFactoryDef =
            localTyper.typed {
              atPos(tree.pos) {
                DefDef(
                  factory,
                  vparamss =>
                    (toConstructor(tree.pos, factory.tpe) /: vparamss) {
                      (fn, vparams) => Apply(fn, vparams map mkArgument)
                    })
              }
            }
          List(transform(tree), caseFactoryDef)
        }

      case ValDef(_, _, _, _) =>
        val tree1 = transform(tree); // important to do before forward reference check
        val ValDef(_, _, _, rhs) = tree1
        if (tree.symbol.hasFlag(LAZY)) {
          assert(tree.symbol.isTerm, tree.symbol)
          val vsym = tree.symbol
          val lazyDefSym = vsym.lazyAccessor
          assert(lazyDefSym != NoSymbol, vsym)
          val ownerTransformer = new ChangeOwnerTraverser(vsym, lazyDefSym)
          val lazyDef = atPos(tree.pos)(
              DefDef(lazyDefSym, vparamss => ownerTransformer(
                if (tree.symbol.owner.isTrait) transform(rhs) // for traits, this is further tranformed in mixins
                else Block(List(
                       Assign(gen.mkAttributedRef(vsym), transform(rhs))),
                       gen.mkAttributedRef(vsym)))))
          log("Made lazy def: " + lazyDef)
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

    override def transform(tree: Tree): Tree = try {

      /* Check whether argument types conform to bounds of type parameters */
      def checkBounds(pre: Type, owner: Symbol, tparams: List[Symbol], argtps: List[Type]): unit = try {
        typer.infer.checkBounds(tree.pos, pre, owner, tparams, argtps, "");
      } catch {
        case ex: TypeError => unit.error(tree.pos, ex.getMessage());
      }

      def isIrrefutable(pat: Tree, seltpe: Type): boolean = {
        val result = pat match {
          case Apply(_, args) =>
            val clazz = pat.tpe.typeSymbol;
            clazz == seltpe.typeSymbol &&
            clazz.isClass && (clazz hasFlag CASE) &&
            List.forall2(
              args,
              clazz.primaryConstructor.tpe.asSeenFrom(seltpe, clazz).paramTypes)(isIrrefutable)
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

      /** Check that a deprecated val or def does not override a
        * concrete, non-deprecated method.  If it does, then
        * deprecation is meaningless.
        */
      def checkDeprecatedOvers() {
        val symbol = tree.symbol
        if (symbol.isDeprecated) {
          val concrOvers =
            symbol.allOverriddenSymbols.filter(sym =>
              !sym.isDeprecated && !(sym hasFlag DEFERRED))
          if(!concrOvers.isEmpty)
            unit.deprecationWarning(
              tree.pos,
              symbol.toString + " overrides concrete, non-deprecated symbol(s):" +
              concrOvers.map(_.fullNameString).mkString("    ", ", ", ""))
        }
      }

      val savedLocalTyper = localTyper
      val sym = tree.symbol
      var result = tree
      tree match {
        case DefDef(mods, name, tparams, vparams, tpt, EmptyTree) if tree.symbol.hasAttribute(definitions.NativeAttr.tpe) =>
          tree.symbol.resetFlag(DEFERRED)
          result = transform(copy.DefDef(tree, mods, name, tparams, vparams, tpt,
                typed(Apply(gen.mkAttributedRef(definitions.Predef_error), List(Literal("native method stub"))))))

        case DefDef(_, _, _, _, _, _) =>
          checkDeprecatedOvers()

        case ValDef(_, _, _, _) =>
          checkDeprecatedOvers()

        case Template(_, _, _) =>
          localTyper = localTyper.atOwner(tree, currentOwner)
          validateBaseTypes(currentOwner)
          checkAllOverrides(currentOwner)

        case TypeTree() =>
          new TypeTraverser {
            def traverse(tp: Type): TypeTraverser = tp match {
              case TypeRef(pre, sym, args) =>
                if (!tp.isHigherKinded) checkBounds(pre, sym.owner, sym.typeParams, args)
                this
              case _ =>
                this
            }
          } traverse tree.tpe

        case TypeApply(fn, args) =>
          checkBounds(NoPrefix, NoSymbol, fn.tpe.typeParams, args map (_.tpe))
          if (sym.isSourceMethod && sym.hasFlag(CASE)) result = toConstructor(tree.pos, tree.tpe)

        case Apply(
          Select(qual, nme.filter),
          List(Function(
            List(ValDef(_, pname, tpt, _)),
            Match(_, CaseDef(pat1, _, _) :: _))))
          if ((pname startsWith nme.CHECK_IF_REFUTABLE_STRING) &&
              isIrrefutable(pat1, tpt.tpe)) =>
            result = qual

        case Apply(fn, args) =>
          checkSensible(tree.pos, fn, args)

        case If(cond, thenpart, elsepart) =>
          cond.tpe match {
            case ConstantType(value) =>
              result = if (value.booleanValue) thenpart else elsepart;
              if (result == EmptyTree) result = Literal(()).setPos(tree.pos).setType(UnitClass.tpe)
            case _ =>
          }

        case New(tpt) =>
          enterReference(tree.pos, tpt.tpe.typeSymbol)

        case Ident(name) =>
          if (sym.isSourceMethod && sym.hasFlag(CASE))
            result = toConstructor(tree.pos, tree.tpe)
          else if (name != nme.WILDCARD && name != nme.WILDCARD_STAR.toTypeName) {
            assert(sym != NoSymbol, tree)//debug
            enterReference(tree.pos, sym)
          }

        case Select(qual, name) =>
          if (currentClass != sym.owner && (sym hasFlag LOCAL)) {
            var o = currentClass
            var hidden = false
            while (!hidden && o != sym.owner && o != sym.owner.moduleClass && !o.isPackage) {
              hidden = o.isTerm || o.isPrivateLocal
              o = o.owner
            }
            if (!hidden) escapedPrivateLocals += sym
          }
          if (sym.isSourceMethod && sym.hasFlag(CASE))
            result = toConstructor(tree.pos, tree.tpe)
          else qual match {
            case Super(qualifier, mix) =>
              val base = qual.symbol;
              //Console.println("super: " + tree + " in " + base);//DEBUG
              assert(!(base.isTrait && sym.isTerm && mix == nme.EMPTY.toTypeName)) // term should have been eliminated by super accessors
            case _ =>
          }
        case _ =>
      }
      result = super.transform(result)
      result match {
        case ClassDef(_, _, _, _)
           | TypeDef(_, _, _, _) =>
          if (result.symbol.isLocal || result.symbol.owner.isPackageClass)
            varianceValidator.traverse(result)
        case _ =>
      }
      localTyper = savedLocalTyper
      result
    } catch {
      case ex: TypeError =>
        if (settings.debug.value) ex.printStackTrace();
        unit.error(tree.pos, ex.getMessage())
        tree
    }
  }
}
