/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.typechecker;

import symtab.Flags._;
import collection.mutable.HashMap;
import util.ListBuffer;
import transform.Transform;

/** Post-attribution checking and transformation.
 *  //todo: check whether we always check type parameter bounds.
 *
 *  This phase performs the following checks.
 *
 *   - All overrides conform to rules.
 *   - All type arguments conform to bounds.
 *   - All type variable uses conform to variance annotations.
 *   - No forward reference to a term symbol extends beyond a value definition.
 *
 *  It performs the following transformations.
 *
 *   - Local modules are replaced by variables and classes
 *   - caseArity, caseElement implementations added to case classes
 *   - equals, and hashCode and toString methods are added to case classes,
 *       unless they are defined in the class or a baseclass
 *       different from java.lang.Object
 *   - toString method is added to case objects,
 *       unless they are defined in the class or a baseclass
 *       different from java.lang.Object
 *   - Calls to case factory methods are replaced by new's.
 */
abstract class RefChecks extends Transform {

  import global._;
  import definitions._;
  import typer.{typed, typedOperator, atOwner};
  import posAssigner.atPos;

  /** the following two members override abstract members in Transform */
  protected val phaseName: String = "refchecks";
  protected def newTransformer(unit: CompilationUnit): Transformer = new RefCheckTransformer(unit);

  class RefCheckTransformer(unit: CompilationUnit) extends Transformer {

// Override checking ------------------------------------------------------------

    /** 1. Check all members of class `clazz' for overriding conditions.
     *  That is for overriding member M and overridden member O:
     *
     *    1.1. M must have the same or stronger access privileges as O.
     *    1.2. O must not be final.
     *    1.3. O is deferred, or M has `override' modifier.
     *    1.4. If O is an immutable value, then so is M.
     *    1.5. Neither M nor O are a parameterized type alias
     *    1.6. If O is a type alias, then M is an alias of O.
     *    1.7. If O is an abstract type then
     *         either M is an abstract type, and M's bounds are sharper than O's bounds.
     *         or M is an unparameterized type alias or class which conforms to O's bounds.
     *    1.8. If O and M are values, then M's type is a subtype of O's type.
     *  2. Check that only abstract classes have deferred members
     *  3. Check that every member with an `override' modifier
     *     overrides some other member.
     */
    private def checkAllOverrides(clazz: Symbol): unit = {

      val self = clazz.thisType;

      def infoString(sym: Symbol) =
	sym.toString() +
	(if (sym.owner == clazz) ""
	 else (sym.locationString +
	       (if (sym.isAliasType) ", which equals " + self.memberInfo(sym)
		else if (sym.isAbstractType) " with bounds " +  self.memberInfo(sym)
		else if (sym.isTerm) " of type " + self.memberInfo(sym)
		else "")));

      /* Check that all conditions for overriding `other' by `member' are met. */
      def checkOverride(clazz: Symbol, member: Symbol, other: Symbol): unit = {
	val pos = if (member.owner == clazz) member.pos else clazz.pos;

	def overrideError(msg: String): unit =
	  if (other.tpe != ErrorType && member.tpe != ErrorType)
	    unit.error(pos, "error overriding " + infoString(other) +
		       ";\n " + infoString(member) + " " + msg);

	def overrideTypeError(): unit = {
	  if (other.tpe != ErrorType && member.tpe != ErrorType) {
	    overrideError("has incompatible type");
	    explainTypes(member.tpe, other.tpe);
	  }
	}

	//System.out.println(infoString(member) + " overrides " + infoString(other) + " in " + clazz);//DEBUG

	// return if we already checked this combination elsewhere
	if (member.owner != clazz) {
	  if ((member.owner isSubClass other.owner) &&
	      ((member hasFlag DEFERRED) || !(other hasFlag DEFERRED))) {
		//System.out.println(infoString(member) + " shadows1 " + infoString(other) " in " + clazz);//DEBUG
		return;
	      }
	  if (clazz.info.parents exists (parent =>
	    (parent.symbol isSubClass other.owner) && (parent.symbol isSubClass member.owner) &&
	    ((member hasFlag DEFERRED) || !(other hasFlag DEFERRED)))) {
	      //System.out.println(infoString(member) + " shadows2 " + infoString(other) + " in " + clazz);//DEBUG
		return;
	    }
	  if (clazz.info.parents forall (parent =>
	    (parent.symbol isSubClass other.owner) == (parent.symbol isSubClass member.owner))) {
	      //System.out.println(infoString(member) + " shadows " + infoString(other) + " in " + clazz);//DEBUG
	      return;
	    }
	}

	if (member hasFlag PRIVATE) { // (1.1)
	  overrideError("has weaker access privileges; it should not be private");
	} else if ((member hasFlag PROTECTED) && !(other hasFlag PROTECTED)) { // 1
	  overrideError("has weaker access privileges; it should not be protected");
	} else if (other hasFlag FINAL) { // (1.2)
	  overrideError("cannot override final member");
	} else if (!(other hasFlag DEFERRED) && !(member hasFlag (OVERRIDE | ABSOVERRIDE))) { // (1.3)
	  overrideError("needs `override' modifier");
	} else if (other.isStable && !member.isStable) { // (1.4)
	  overrideError("needs to be an immutable value");
	} else {
	  if (other.isAliasType) {
	    if (!member.typeParams.isEmpty) // (1.5)
	      overrideError("may not be parameterized");
	    if (!other.typeParams.isEmpty) // (1.5)
	      overrideError("may not override parameterized type");
	    if (!(self.memberType(member) =:= self.memberType(other))) // (1.6)
	      overrideTypeError();
	  } else if (other.isAbstractType) {
	    if (!member.typeParams.isEmpty) // (1.7)
	      overrideError("may not be parameterized");
	    if (!(self.memberInfo(other).bounds containsType self.memberInfo(member))) // (1.7)
	      overrideTypeError();
	  } else if (other.isTerm) {
	    if (!(self.memberInfo(member) <:< (self.memberInfo(other)))) // 8
	      overrideTypeError();
	  }
	}
      }

      // 1. Check all members for overriding conditions.
      for (val bc <- clazz.info.baseClasses.tail; val other <- bc.info.decls.toList)
	if (!other.isClass && !(other hasFlag PRIVATE) && !other.isConstructor) {
	  val member = clazz.tpe.member(other.name) filter
	    (sym => sym.owner != other.owner &&
             (sym.isType || (self.memberType(sym) matches self.memberType(other))));
	  if (member hasFlag OVERLOADED) {
	    val alt1 = member.alternatives.head;
	    val alt2 = member.alternatives.tail.head;
	    val pos = if (alt1.owner == clazz) alt1.pos
		      else if (alt2.owner == clazz) alt2.pos
		      else clazz.pos;
	    unit.error(pos,
	      "ambiguous override: both " + infoString(alt1) +
	      "\n and " + infoString(alt2) +
	      "\n override " + infoString(other));
	  } else if (member != NoSymbol && !(member hasFlag LOCAL)) {
	    member.flags = member.flags | ACCESSED;
	    checkOverride(clazz, member, other);
	  }
	}

      // 2. Check that only abstract classes have deferred members
      if (clazz.isClass && !(clazz hasFlag ABSTRACT)) {
	def abstractClassError(msg: String): unit = {
	  unit.error(clazz.pos,
	    (if (clazz.isAnonymousClass || clazz.isModuleClass) "object creation impossible"
	     else clazz.toString() + " needs to be abstract") + ", since " + msg);
	  clazz.setFlag(ABSTRACT);
	}
	for (val member <- clazz.tpe.members)
	  if (member hasFlag DEFERRED) {
	    abstractClassError(
	      infoString(member) + " is not defined" +
	      (if (member hasFlag MUTABLE)
		"\n(Note that variables need to be initialized to be defined)" else ""))
	  } else if (member.isIncompleteIn(clazz)) {
	    val other = member.superSymbol(clazz);
	    abstractClassError(
	      infoString(member) + " is marked `abstract' and `override'" +
	      (if (other != NoSymbol)
		" and overrides incomplete superclass member " + infoString(other)
	       else ""))
	  }
      }

      // 3. Check that every defined member with an `override' modifier overrides some other member.
      for (val member <- clazz.info.decls.toList)
	if ((member hasFlag (OVERRIDE | ABSOVERRIDE)) &&
	    (clazz.info.baseClasses.tail forall (bc => member.overriddenSymbol(bc) == NoSymbol))) {
	  System.out.println(clazz.info.baseClasses.tail);//debug
	  unit.error(member.pos, member.toString() + " overrides nothing");
	  member resetFlag OVERRIDE
	}
    }

  // Basetype Checking --------------------------------------------------------

    /** 1. Check that later type instances in the base-type sequence
     *     are subtypes of earlier type instances of the same trait.
     *  2. Check that case classes do not inherit from case classes.
     *  3. Check that at most one base type is a case-class.
     */
    private def validateBaseTypes(clazz: Symbol): unit = {
      val seenTypes = new Array[Type](clazz.info.closure.length);
      var seenCaseClass = if (clazz hasFlag CASE) clazz else NoSymbol;

      def validateTypes(tps: List[Type], includeSuper: boolean): unit = {
	if (!tps.isEmpty) {
	  for (val tp <- tps.tail.reverse) validateType(tp, false);
	  if (includeSuper) validateType(tps.head, true);
	}
      }

      def validateType(tp: Type, includeSuper: boolean): unit = {
	val baseClass = tp.symbol;
	if (baseClass.isClass) {
	  val index = clazz.info.closurePos(baseClass);
	  if (index >= 0) {
	    if (seenTypes(index) != null && !(seenTypes(index) <:< tp))
	      unit.error(clazz.pos, "illegal inheritance;\n " + clazz +
			 " inherits different type instances of " + baseClass +
			 ":\n" + tp + " and " + seenTypes(index));
	    seenTypes(index) = tp;
	    // check that case classes do not inherit from case classes
	    if (baseClass hasFlag CASE) {
	      if (seenCaseClass != NoSymbol && seenCaseClass != baseClass)
		unit.error(clazz.pos, "illegal combination of case " +
			   seenCaseClass + " and case " + baseClass + " in one object");
	      seenCaseClass = baseClass
	    }
	  }
	  validateTypes(tp.parents, includeSuper);
	}
      }

      validateTypes(clazz.info.parents, true);
    }

  // Variance Checking --------------------------------------------------------

    private val ContraVariance = -1;
    private val NoVariance = 0;
    private val CoVariance = 1;
    private val AnyVariance = 2;

    /** Check variance of type variables in this type
     */
    private def validateVariance(base: Symbol, all: Type, variance: int): unit = {

      def varianceString(variance: int): String =
	if (variance == 1) "covariant"
	else if (variance == -1) "contravariant"
	else "invariant";

      def relativeVariance(tvar: Symbol): int = {
	val clazz = tvar.owner;
	var sym = base;
	var state = CoVariance;
	while (sym != clazz && state != AnyVariance) {
	  //System.out.println("flip: " + sym + " " + sym.isParameter());//DEBUG
	  if ((sym hasFlag PARAM) && !sym.owner.isConstructor) state = -state;
	  else if (!sym.owner.isClass) state = AnyVariance;
	  else if (sym.isAliasType) state = NoVariance;
	  sym = sym.owner;
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
	case SingleType(pre, sym) =>
	  validateVariance(pre, variance)
	case TypeRef(pre, sym, args) =>
	  if (sym.variance != NoVariance) {
	    val v = relativeVariance(sym);
	    if (v != AnyVariance && sym.variance != v * variance) {
	      //System.out.println("relativeVariance(" + base + "," + sym + ") = " + v);//DEBUG
	      unit.error(base.pos,
			 varianceString(sym.variance) + " " + sym +
			 " occurs in " + varianceString(v * variance) +
			 " position in type " + all + " of " + base);
	    }
	  }
	  validateVariance(pre, variance);
	  validateVarianceArgs(args, variance, sym.typeParams);
	case ClassInfoType(parents, decls, symbol) =>
	  validateVariances(parents, variance);
	case RefinedType(parents, decls) =>
	  validateVariances(parents, variance);
	case TypeBounds(lo, hi) =>
	  validateVariance(lo, -variance);
	  validateVariance(hi, variance);
	case MethodType(formals, result) =>
	  validateVariance(result, variance);
	case PolyType(tparams, result) =>
	  validateVariance(result, variance);
      }

      def validateVariances(tps: List[Type], variance: int): unit =
	tps foreach (tp => validateVariance(tp, variance));

      def validateVarianceArgs(tps: List[Type], variance: int, tparams: List[Symbol]): unit =
	(tps zip tparams) foreach {
	  case Pair(tp, tparam) => validateVariance(tp, variance * tparam.variance)
	}

      validateVariance(all, variance)
    }

// Forward reference checking ---------------------------------------------------

    class LevelInfo(val outer: LevelInfo) {
      val scope: Scope = if (outer == null) new Scope() else new Scope(outer.scope);
      var maxindex: int = Integer.MIN_VALUE;
      var refpos: int = _;
      var refsym: Symbol = _;
    }

    private var currentLevel: LevelInfo = null;
    private val symIndex = new HashMap[Symbol, int];

    private def pushLevel(): unit =
      currentLevel = new LevelInfo(currentLevel);

    private def popLevel(): unit =
      currentLevel = currentLevel.outer;

    private def enterSyms(stats: List[Tree]): unit = {
      var index = -1;
      for (val stat <- stats) {
	index = index + 1;
	stat match {
          case ClassDef(_, _, _, _, _) | DefDef(_, _, _, _, _, _) if (stat.symbol.isLocal) =>
	    currentLevel.scope.enter(newScopeEntry(stat.symbol, currentLevel.scope));
	    symIndex(stat.symbol) = index;
          case _ =>
	}
      }
    }

    private def enterReference(pos: int, sym: Symbol): unit =
      if (sym.isLocal && sym.isTerm) {
	val e = currentLevel.scope.lookupEntry(sym.name);
	if (e != null && sym == e.sym) {
          var l = currentLevel;
          while (l.scope != e.owner) l = l.outer;
	  val symindex = symIndex(sym);
	  if (l.maxindex < symindex) {
	    l.refpos = pos;
	    l.refsym = sym;
	    l.maxindex = symindex;
	  }
	}
      }

// Adding synthetic methods --------------------------------------------------------------

    private def addSyntheticMethods(templ: Template, clazz: Symbol): Template = {

      def hasImplementation(name: Name): boolean = {
	val sym = clazz.info.nonPrivateMember(name);
	sym.isTerm &&
	(sym.owner == clazz ||
	 !(ObjectClass isSubClass sym.owner) && !(sym hasFlag DEFERRED));
      }

      def syntheticMethod(name: Name, flags: int, tpe: Type) = {
	val method = clazz.newMethod(clazz.pos, name) setFlag (flags | OVERRIDE) setInfo tpe;
	clazz.info.decls.enter(method);
	method
      }

      def caseElementMethod: Tree = {
	val method = syntheticMethod(
	  nme.caseElement, FINAL, MethodType(List(IntClass.tpe), AnyClass.tpe));
	val caseFields = clazz.caseFieldAccessors map gen.mkRef;
	typed(
	  DefDef(method, vparamss =>
	    if (caseFields.isEmpty) Literal(Constant(null))
	    else {
	      var i = caseFields.length;
	      var cases = List(CaseDef(Ident(nme.WILDCARD), EmptyTree, Literal(Constant(null))));
	      for (val field <- caseFields.reverse) {
		i = i - 1; cases = CaseDef(Literal(Constant(i)), EmptyTree, field) :: cases
	      }
	      Match(Ident(vparamss.head.head), cases)
	    }))
      }

      def caseArityMethod: Tree = {
	val method = syntheticMethod(nme.caseArity, FINAL, PolyType(List(), IntClass.tpe));
	typed(DefDef(method, vparamss => Literal(Constant(clazz.caseFieldAccessors.length))))
      }

      def caseNameMethod: Tree = {
	val method = syntheticMethod(nme.caseName, FINAL, PolyType(List(), StringClass.tpe));
	typed(DefDef(method, vparamss => Literal(Constant(clazz.name.decode))))
      }

      def moduleToStringMethod: Tree = {
	val method = syntheticMethod(nme.toString_, FINAL, MethodType(List(), StringClass.tpe));
	typed(DefDef(method, vparamss => Literal(Constant(clazz.name.decode))))
      }

      def tagMethod: Tree = {
	val method = syntheticMethod(nme.tag, FINAL, MethodType(List(), IntClass.tpe));
	typed(DefDef(method, vparamss => Literal(Constant(clazz.tag))))
      }

      def forwardingMethod(name: Name): Tree = {
	val target = getMember(ScalaRunTimeModule, "_" + name);
	val method = syntheticMethod(
	  name, 0, MethodType(target.tpe.paramTypes.tail, target.tpe.resultType));
	typed(DefDef(method, vparamss =>
	  Apply(gen.mkRef(target), This(clazz) :: (vparamss.head map Ident))));
      }

      def readResolveMethod: Tree = {
	// !!! the synthetic method "readResolve" should be private,
	// but then it is renamed !!!
	val method = syntheticMethod(nme.readResolve, PROTECTED, MethodType(List(), ObjectClass.tpe));
	typed(DefDef(method, vparamss => gen.mkRef(clazz.sourceModule)))
      }

      val ts = new ListBuffer[Tree];
      if (clazz hasFlag CASE) {
	ts += tagMethod;
	if (clazz.isModuleClass) {
	  ts += moduleToStringMethod;
	  if (clazz.isSubClass(SerializableClass)) {
	    // If you serialize a singleton and then deserialize it twice,
	    // you will have two instances of your singleton, unless you implement
	    // the readResolve() method (see http://www.javaworld.com/javaworld/
	    // jw-04-2003/jw-0425-designpatterns_p.html)
	    ts += readResolveMethod;
	  }
	} else {
	  ts += caseElementMethod;
	  ts += caseArityMethod;
	  ts += caseNameMethod;
	  if (!hasImplementation(nme.equals_)) ts += forwardingMethod(nme.equals_);
	  if (!hasImplementation(nme.hashCode_)) ts += forwardingMethod(nme.hashCode_);
	  if (!hasImplementation(nme.toString_)) ts += forwardingMethod(nme.toString_);
	}
      }
      val synthetics = ts.toList;
      copy.Template(
	templ, templ.parents, if (synthetics.isEmpty) templ.body else templ.body ::: synthetics)
    }

// Transformation ------------------------------------------------------------

    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      pushLevel();
      enterSyms(stats);
      var index = -1;
      val stats1 = stats flatMap { stat => index = index + 1; transformStat(stat, index) }
      popLevel();
      stats1
    }

    def transformStat(tree: Tree, index: int): List[Tree] = tree match {
      case ModuleDef(_, name, impl) =>
	val sym = tree.symbol;
        //val localTyper = typer.atOwner(currentOwner);
	val cdef = typed(ClassDef(sym.moduleClass, impl));
	if (sym.isStatic) List(transform(cdef))
	else {
          val moduleType = sym.tpe;

          // var m$: T = null; or, if class member: local var m$: T = _;
          val mvar = currentOwner.newVariable(sym.pos, name.toString() + "$") setInfo moduleType;
          if (currentOwner.isClass) {
	    mvar setFlag (PRIVATE | LOCAL | SYNTHETIC);
	    sym.owner.info.decls.enter(mvar);
	  }
          val vdef = typed(ValDef(mvar, if (sym.isLocal) Literal(Constant(null)) else EmptyTree));

          // def m: T = { if (m$ == null) m$ = new m$class; m$ }
          sym.setFlag(METHOD | STABLE);
          sym.setInfo(PolyType(List(), moduleType));
          val ddef = typed(
            DefDef(sym, vparamss =>
              Block(
		List(
		  If(
                    Apply(Select(Ident(mvar), nme.EQ), List(Literal(Constant(null)))),
                    Assign(Ident(mvar), New(TypeTree(moduleType), List(List()))),
                    EmptyTree)),
		Ident(mvar))));
          transformTrees(List(cdef, vdef, ddef))
	}

      case ValDef(_, _, _, _) =>
	val tree1 = transform(tree); // important to do before forward reference check
	//todo: handle variables
	if (tree.symbol.isLocal && index <= currentLevel.maxindex) {
	  if (settings.debug.value) System.out.println(currentLevel.refsym);
	  unit.error(currentLevel.refpos, "forward reference extends over definition of " + tree.symbol);
	}
	List(tree1)

      case Import(_, _) =>
	List()

      case _ =>
	List(transform(tree))
    }

    override def transform(tree: Tree): Tree = try {

      /* Convert a reference of a case factory to a new of the class it produces. */
      def toConstructor: Tree = {
	var tpe = tree.tpe;
	while (!tpe.symbol.isClass) tpe = tpe.resultType;
	assert(tpe.symbol hasFlag CASE);
	typedOperator(atPos(tree.pos)(Select(New(TypeTree(tpe)), tpe.symbol.primaryConstructor)));
      }

      /* Check whether argument types conform to bounds of type parameters */
      def checkBounds(tparams: List[Symbol], argtps: List[Type]): unit = try {
	infer.checkBounds(tree.pos, tparams, argtps, "");
      } catch {
	case ex: TypeError => unit.error(tree.pos, ex.getMessage());
      }

      val sym = tree.symbol;
      var result = tree;
      tree match {
	case ClassDef(mods, name, tparams, tpe, impl) =>
	  validateVariance(sym, sym.info, CoVariance);
	  validateVariance(sym, sym.typeOfThis, CoVariance);
	  result = copy.ClassDef(
	    tree, mods, name, tparams, tpe, addSyntheticMethods(impl, tree.symbol))

	case DefDef(_, _, _, _, _, _) =>
	  validateVariance(sym, sym.tpe, CoVariance);

	case ValDef(_, _, _, _) =>
	  validateVariance(sym, sym.tpe, if ((sym.flags & MUTABLE) != 0) NoVariance else CoVariance);

	case AbsTypeDef(_, _, _, _) =>
	  validateVariance(sym, sym.info, CoVariance);

	case AliasTypeDef(_, _, _, _) =>
	  validateVariance(sym, sym.info, CoVariance);

	case Template(_, _) =>
	  validateBaseTypes(currentOwner);
	  checkAllOverrides(currentOwner);

	case TypeTree() =>
	  new TypeTraverser {
	    def traverse(tp: Type) = tp match {
	      case TypeRef(pre, sym, args) => checkBounds(sym.typeParams, args); this
	      case _ => this
	    }
	  } traverse tree.tpe

	case TypeApply(fn, args) =>
	  checkBounds(fn.tpe.typeParams, args map (.tpe));
	  if (sym.isSourceMethod && sym.hasFlag(CASE)) result = toConstructor;

	case New(tpt) =>
	  enterReference(tree.pos, tpt.tpe.symbol);

	case Ident(name) =>
	  if (sym.isSourceMethod && sym.hasFlag(CASE))
	    result = toConstructor
	  else if (name != nme.WILDCARD && name != nme.WILDCARD_STAR.toTypeName) {
	    sym setFlag ACCESSED;
	    assert(sym != NoSymbol, tree);//debug
	    enterReference(tree.pos, sym);
	  }

	case Select(qual, name) =>
	  if (sym.isSourceMethod && sym.hasFlag(CASE))
	    result = toConstructor
	  else {
	    sym setFlag ACCESSED;
	    if (!treeInfo.isSelf(qual, currentOwner.enclClass)) sym.flags = sym.flags | SELECTOR;
	    if (sym hasFlag DEFERRED) {
	      qual match {
		case Super(qualifier, mixin) =>
		  val base = currentOwner.enclClass;
		  val member = sym.overridingSymbol(base);
		  if (mixin != nme.EMPTY.toTypeName || member == NoSymbol ||
		      !((member hasFlag ABSOVERRIDE) && member.isIncompleteIn(base)))
		    unit.error(tree.pos, "symbol accessed from super may not be abstract");
		case _ =>
	      }
	    }
	  }

	case _ =>
      }
      super.transform(result)
    } catch {
      case ex: TypeError =>
	if (settings.debug.value) ex.printStackTrace();
	unit.error(tree.pos, ex.getMessage());
	tree
    }
  }
}
