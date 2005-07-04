/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.typechecker;

import nsc.util.ListBuffer;
import symtab.Flags._;
import scala.tools.util.Position;

/** Methods to create symbols and to enter them into scopes. */
abstract class Typers: Analyzer {
  import global._;
  import definitions._;
  import posAssigner.atPos;

  var appcnt = 0;
  var idcnt = 0;
  var selcnt = 0;
  var implcnt = 0;

  class TyperPhase(prev: Phase) extends StdPhase(prev) {
    def name = "typer";
    val global: Typers.this.global.type = Typers.this.global;
    def apply(unit: CompilationUnit): unit =
      unit.body = newTyper(startContext.make(unit)).typed(unit.body)
  }

  def newTyper(context: Context): Typer = new Typer(context);

  class Typer(context0: Context) {
    import context0.unit;

    val infer = new Inferencer(context0) {
      override def isCoercible(tp: Type, pt: Type): boolean =
        context0.reportGeneralErrors && // this condition prevents chains of views
	inferView(Position.NOPOS, tp, pt, false) != EmptyTree
    }

    private def inferView(pos: int, from: Type, to: Type, reportAmbiguous: boolean): Tree = {
      if (settings.debug.value) log("infer view from " + from + " to " + to);//debug
      val res = inferImplicit(pos, functionType(List(from), to), true, reportAmbiguous);
      res
    }

    private def inferView(pos: int, from: Type, name: Name, reportAmbiguous: boolean): Tree = {
      val to = refinedType(List(WildcardType), NoSymbol);
      val psym = (if (name.isTypeName) to.symbol.newAbstractType(pos, name)
		  else to.symbol.newValue(pos, name)) setInfo WildcardType;
      to.decls.enter(psym);
      inferView(pos, from, to, reportAmbiguous)
    }

    import infer._;

    private var namerCache: Namer = null;
    def namer = {
      if (namerCache == null || namerCache.context != context) namerCache = new Namer(context);
      namerCache
    }

    private var context = context0;

    /** Mode constants
     */
    val NOmode        = 0x000;
    val EXPRmode      = 0x001;   // these 3 modes are mutually exclusive.
    val PATTERNmode   = 0x002;
    val TYPEmode      = 0x004;

    val INCONSTRmode  = 0x008;   // orthogonal to above. When set we are
                                 // in the body of a constructor

    val FUNmode       = 0x10;    // orthogonal to above. When set
                                 // we are looking for a method or constructor

    val POLYmode      = 0x020;   // orthogonal to above. When set
                                 // expression types can be polymorphic.

    val QUALmode      = 0x040;   // orthogonal to above. When set
                                 // expressions may be packages and
                                 // Java statics modules.

    val TAPPmode      = 0x080;   // Set for the function/type constructor part
	                         // of a type application. When set we do not
	                         // decompose PolyTypes.

    val SUPERCONSTRmode = 0x100; // Set for the `super' in a superclass constructor call
                                 // super.<init>

    private val stickyModes: int  = EXPRmode | PATTERNmode | TYPEmode;

    /** Report a type error.
     *  @param pos    The position where to report the error
     *  @param ex     The exception that caused the error */
    def reportTypeError(pos: int, ex: TypeError): unit = {
      if (settings.debug.value) ex.printStackTrace();
      ex match {
	case CyclicReference(sym, info: TypeCompleter) =>
	  info.tree match {
	    case ValDef(_, _, tpt, _) if (tpt.tpe == null) =>
	      error(pos, "recursive " + sym + " needs type")
	    case DefDef(_, _, _, _, tpt, _) if (tpt.tpe == null) =>
	      error(pos, "recursive " + sym + " needs result type")
	    case _ =>
	      error(pos, ex.getMessage())
	  }
	case _ =>
	  error(pos, ex.getMessage())
      }
    }

    /** Check that tree is a stable expression.
     */
    def checkStable(tree: Tree): Tree =
      if (treeInfo.isPureExpr(tree) || tree.tpe.isError) tree;
      else errorTree(tree, "stable identifier required, but " + tree + " found.");

    /** Check that type of given tree does not contain local or private components
     */
    object checkNoEscaping extends TypeMap {
      private var owner: Symbol = _;
      private var scope: Scope = _;
      private var badSymbol: Symbol = _;

      /** Check that type `tree' does not refer to private components unless itself is wrapped
       *  in something private (`owner' tells where the type occurs). */
      def privates[T <: Tree](owner: Symbol, tree: T): T = {
	check(owner, EmptyScope, tree);
      }

      /**  Check that type `tree' does not refer to entities defined in scope `scope'. */
      def locals[T <: Tree](scope: Scope, tree: T): T = check(NoSymbol, scope, tree);

      def check[T <: Tree](owner: Symbol, scope: Scope, tree: T): T = {
        this.owner = owner;
        this.scope = scope;
        badSymbol = NoSymbol;
	assert(tree.tpe != null, tree);//debug
        apply(tree.tpe);
        if (badSymbol == NoSymbol) tree
        else {
          error(tree.pos,
                (if (badSymbol.hasFlag(PRIVATE)) "private " else "") + badSymbol +
                " escapes its defining scope as part of type " + tree.tpe);
          setError(tree)
        }
      }
      override def apply(t: Type): Type = {
        def checkNoEscape(sym: Symbol): unit = {
          if (sym.hasFlag(PRIVATE)) {
            var o = owner;
            while (o != NoSymbol && o != sym.owner && !o.isLocal && !o.hasFlag(PRIVATE))
              o = o.owner;
            if (o == sym.owner) badSymbol = sym
          } else if (sym.owner.isTerm) {
            val e = scope.lookupEntry(sym.name);
            if (e != null && e.sym == sym && e.owner == scope) badSymbol = e.sym
          }
        }
        if (badSymbol == NoSymbol)
          t match {
            case TypeRef(_, sym, _) => checkNoEscape(sym)
            case SingleType(_, sym) => checkNoEscape(sym)
            case _ =>
          }
        mapOver(t)
      }
    }

    def reenterValueParams(vparamss: List[List[ValDef]]): unit =
      for (val vparams <- vparamss; val vparam <- vparams) context.scope enter vparam.symbol;

    def reenterTypeParams(tparams: List[AbsTypeDef]): List[Symbol] =
      for (val tparam <- tparams) yield { context.scope enter tparam.symbol; tparam.symbol }

    def attrInfo(attr: Tree): AttrInfo = attr match {
      case Apply(Select(New(tpt), nme.CONSTRUCTOR), args) =>
        Pair(tpt.tpe, args map {
          case Literal(value) =>
            value
          case arg =>
            error(arg.pos, "attribute argument needs to be a constant; found: " + arg);
        })
    }

    /** Post-process an identifier or selection node, performing the following:
     *  (1) Check that non-function pattern expressions are stable
     *  (2) Check that packages and static modules are not used as values
     *  (3) Turn tree type into stable type if possible and required by context. */
    private def stabilize(tree: Tree, pre: Type, mode: int, pt: Type): Tree = {
      if (tree.symbol.hasFlag(OVERLOADED) && (mode & FUNmode) == 0)
	inferExprAlternative(tree, pt);
      val sym = tree.symbol;
      if ((mode & (PATTERNmode | FUNmode)) == PATTERNmode && tree.isTerm) // (1)
        checkStable(tree)
      else if ((mode & (EXPRmode | QUALmode)) == EXPRmode && !sym.isValue) // (2)
        errorTree(tree, sym.toString() + " is not a value");
      else if (sym.isStable && pre.isStable && tree.tpe.symbol != ByNameParamClass &&
	       (pt.isStable || (mode & QUALmode) != 0 && !sym.isConstant ||
		sym.isModule && !sym.tpe.isInstanceOf[MethodType])) {
	tree.setType(singleType(pre, sym))
      } else tree
    }

    def stabilizeFun(tree: Tree, mode: int, pt: Type): Tree = {
      val sym = tree.symbol;
      val pre = tree match {
	case Select(qual, _) => qual.tpe
	case _ => NoPrefix
      }
      if (tree.tpe.isInstanceOf[MethodType] && pre.isStable &&
	  (pt.isStable || (mode & QUALmode) != 0 && !sym.isConstant || sym.isModule)) {
	assert(sym.tpe.paramTypes.isEmpty);
	tree.setType(MethodType(List(), singleType(pre, sym)))
      } else tree
    }

    /** Perform the following adaptations of expression, pattern or type `tree' wrt to
     *  given mode `mode' and given prototype `pt':
     *  (0) Convert expressions with constant types to literals
     *  (1) Resolve overloading, unless mode contains FUNmode
     *  (2) Apply parameterless functions
     *  (3) Apply polymorphic types to fresh instances of their type parameters and
     *      store these instances in context.undetparams,
     *      unless followed by explicit type application.
     *  (4) Do the following to unapplied methods used as values:
     *  (4.1) If the method has only implicit parameters pass implicit arguments
     *  (4.2) otherwise, convert to function by eta-expansion,
     *        except if the method is a constructor, in which case we issue an error.
     *  (5) Convert a class type that serves as a constructor in a pattern as follows:
     *  (5.1) If this type refers to a case class, set tree's type to the unique
     *        instance of its primary constructor that is a subtype of the expected type.
     *  (5.2) Otherwise, if this type is a subtype of scala.Seq[A], set trees' type
     *        to a method type from a repeated parameter sequence type A* to the expected type.
     *  (6) Convert all other types to TypeTree nodes.
     *  (7) When in TYPEmode nut not FUNmode, check that types are fully parameterized
     *  (8) When in both EXPRmode and FUNmode, add apply method calls to values of object type.
     *  (9) If there are undetermined type variables and not POLYmode, infer expression instance
     *  Then, if tree's type is not a subtype of expected type, try the following adaptations:
     *  (10) If the expected type is byte, short or char, and the expression
     *      is an integer fitting in the range of that type, convert it to that type.
     *  (11) Widen numeric literals to their expected type, if necessary
     *  (12) When in mode EXPRmode, convert E to { E; () } if expected type is Scala.unit.
     *  (13) When in mode EXPRmode, apply a view
     *  If all this fails, error
     */
//    def adapt(tree: Tree, mode: int, pt: Type): Tree = {
    private def adapt(tree: Tree, mode: int, pt: Type): Tree = tree.tpe match {
      case ct @ ConstantType(value) if ((mode & TYPEmode) == 0 && (ct <:< pt)) => // (0)
	copy.Literal(tree, value)
      case OverloadedType(pre, alts) if ((mode & FUNmode) == 0) => // (1)
	inferExprAlternative(tree, pt);
	adapt(tree, mode, pt)
      case PolyType(List(), restpe) => // (2)
	adapt(tree setType restpe, mode, pt);
      case TypeRef(_, sym, List(arg))
      if ((mode & EXPRmode) != 0 && sym == ByNameParamClass) => // (2)
	adapt(tree setType arg, mode, pt);
      case PolyType(tparams, restpe) if ((mode & TAPPmode) == 0) => // (3)
	if (settings.debug.value && tree.symbol != null) log("adapting " + tree + " " + tree.symbol.tpe + " " + tree.symbol.getClass() + " " + tree.symbol.hasFlag(CASE));//debug
	val tparams1 = cloneSymbols(tparams);
        val tree1 = if (tree.isType) tree
                    else TypeApply(tree, tparams1 map (tparam =>
                      TypeTree() setPos tree.pos setType tparam.tpe)) setPos tree.pos;
	context.undetparams = context.undetparams ::: tparams1;
	adapt(tree1 setType restpe.substSym(tparams, tparams1), mode, pt)
      case mt: ImplicitMethodType if ((mode & (EXPRmode | FUNmode)) == EXPRmode) => // (4.1)
	typed(applyImplicitArgs(tree), mode, pt)
      case mt: MethodType if ((mode & (EXPRmode | FUNmode)) == EXPRmode &&
	                      isCompatible(tree.tpe, pt)) => // (4.2)
	if (tree.symbol.isConstructor) errorTree(tree, "missing arguments for " + tree.symbol)
	else typed(etaExpand(tree), mode, pt)
      case _ =>
	if (tree.isType) {
	  val clazz = tree.tpe.symbol;
	  if ((mode & PATTERNmode) != 0) { // (5)
	    if (tree.tpe.isInstanceOf[MethodType]) {
	      tree // everything done already
	    } else {
	      clazz.initialize;
	      if (clazz.hasFlag(CASE)) {   // (5.1)
		val tree1 = TypeTree() setPos tree.pos
		    setType
		      clazz.primaryConstructor.tpe.asSeenFrom(tree.tpe.prefix, clazz.owner);
		// tree.tpe.prefix.memberType(clazz.primaryConstructor); //!!!
		inferConstructorInstance(tree1, clazz.unsafeTypeParams, pt);
		tree1
	      } else if (clazz.isSubClass(SeqClass)) { // (5.2)
		pt.baseType(clazz).baseType(SeqClass) match {
		  case TypeRef(pre, seqClass, args) =>
		    tree.setType(MethodType(List(typeRef(pre, RepeatedParamClass, args)), pt))
		  case NoType =>
		    errorTree(tree, "expected pattern type " + pt +
			      " does not conform to sequence " + clazz)
		}
	      } else {
		System.out.println("bad: " + clazz + ":" + tree.tpe + " " + flagsToString(clazz.flags) + clazz.hasFlag(CASE));//debug
		errorTree(tree, clazz.toString() + " is neither a case class nor a sequence class")
	      }
	    }
	  } else if ((mode & FUNmode) != 0) {
	    tree
	  } else if (tree.hasSymbol && !tree.symbol.unsafeTypeParams.isEmpty) { // (7)
            errorTree(tree, "" + clazz + " takes type parameters");
          } else tree match { // (6)
            case TypeTree() => tree
            case _ => TypeTree() setPos tree.pos setType tree.tpe
          }
	} else if ((mode & (EXPRmode | FUNmode)) == (EXPRmode | FUNmode) &&
                   ((mode & TAPPmode) == 0 || tree.tpe.typeParams.isEmpty) &&
		   tree.tpe.member(nme.apply).filter(m => m.tpe.paramSectionCount > 0) != NoSymbol) { // (8)
	  typed(Select(tree, nme.apply) setPos tree.pos, mode, pt)
	} else if (!context.undetparams.isEmpty & (mode & POLYmode) == 0) { // (9)
	    val tparams = context.undetparams;
	    context.undetparams = List();
	    inferExprInstance(tree, tparams, pt);
	    tree
	} else if (tree.tpe <:< pt) {
	  tree
	} else {
	  val tree1 = constfold(tree, pt); // (10) (11)
	  if (tree1.tpe <:< pt) adapt(tree1, mode, pt)
	  else {
            if ((mode & (EXPRmode | FUNmode)) == EXPRmode) {
              pt match {
                case TypeRef(_, sym, _) =>
                  // note: was if (pt.symbol == UnitClass) but this leads to a potentially
                  // infinite expansion if pt is constant type ()
                  if (sym == UnitClass && tree.tpe <:< AnyClass.tpe) // (12)
                    return typed(atPos(tree.pos)(Block(List(tree), Literal(()))), mode, pt)
                case _ =>
              }
	      if (context.reportGeneralErrors) { // (13); the condition prevents chains of views
	        val coercion = inferView(tree.pos, tree.tpe, pt, true);
	        if (coercion != EmptyTree)
		  return typed(Apply(coercion, List(tree)) setPos tree.pos, mode, pt);
	      }
            }
            if (settings.debug.value) log("error tree = " + tree);
	    typeErrorTree(tree, tree.tpe, pt)
          }
	}
    }
//      System.out.println("adapt " + tree + ":" + tree.tpe + ", mode = " + mode + ", pt = " + pt);
//      adapt(tree, mode, pt)
//    }

    private def completeSuperType(supertpt: Tree, tparams: List[Symbol], vparamss: List[List[ValDef]], superargs: List[Tree]): Type = {
      tparams foreach context.scope.enter;
      namer.enterValueParams(context.owner, vparamss);
      val newTree = New(supertpt) setType
	PolyType(tparams, appliedType(supertpt.tpe, tparams map (.tpe)));
      val tree = typed(atPos(supertpt.pos)(Apply(Select(newTree, nme.CONSTRUCTOR), superargs)));
      if (settings.debug.value) log("superconstr " + tree + " co = " + context.owner);//debug
      tree.tpe
    }

    def parentTypes(templ: Template): List[Tree] = {
      var supertpt = typedTypeConstructor(templ.parents.head);
      var mixins = templ.parents.tail map typedType;
      // If first parent is trait, make it first mixin and add its superclass as first parent
      while (supertpt.tpe.symbol != null && supertpt.tpe.symbol.initialize.isTrait) {
	mixins = typedType(supertpt) :: mixins;
        supertpt = TypeTree(supertpt.tpe.parents.head) setPos supertpt.pos;
      }
      if (supertpt.hasSymbol) {
        val tparams = supertpt.symbol.typeParams;
        if (!tparams.isEmpty) {
          val constr @ DefDef(_, _, _, vparamss, _, Apply(_, superargs)) =
	    treeInfo.firstConstructor(templ.body);
	  val outercontext = context.outer.outer;
          supertpt = TypeTree(
            newTyper(context.outer.outer.makeNewScope(constr, context.outer.outer.owner))
              .completeSuperType(
                supertpt,
                tparams,
                vparamss map (.map(.duplicate.asInstanceOf[ValDef])),
                superargs map (.duplicate))) setPos supertpt.pos;
        }
      }
      //System.out.println("parents(" + context.owner + ") = " + supertpt :: mixins);//DEBUG
      List.mapConserve(supertpt :: mixins)(tpt => checkNoEscaping.privates(context.owner, tpt))
    }

    /** Check that
     *  - all parents are class types,
     *  - first parent cluss is not a trait; following classes are traits,
     *  - final classes are not inherited,
     *  - sealed classes are only inherited by classes which are
     *    nested within definition of base class, or that occur within same
     *    statement sequence,
     *  - self-type of current class is a subtype of self-type of each parent class.
     *  - no two parents define same symbol.
     */
    def validateParentClasses(parents: List[Tree], selfType: Type): unit = {
      var c = context;
      do { c = c.outer } while (c.owner == context.owner);
      val defscope = c.scope;

      def validateParentClass(parent: Tree, isFirst: boolean): unit =
	if (!parent.tpe.isError) {
	  val psym = parent.tpe.symbol.initialize;
	  if (!psym.isClass)
	    error(parent.pos, "class type expected");
	  else if (!isFirst && !psym.isTrait)
	    error(parent.pos, "" + psym + " is not a trait; cannot be used as mixin");
	  else if (psym.hasFlag(FINAL))
	    error(parent.pos, "illegal inheritance from final class");
	  else if (psym.isSealed) {
	    // are we in same scope as base type definition?
	    val e = defscope.lookupEntry(psym.name);
	    if (!(e.sym == psym && e.owner == defscope)) {
	      // we are not within same statement sequence
	      var c = context;
	      while (c != NoContext && c.owner !=  psym) c = c.outer.enclClass;
	      if (c == NoContext) error(parent.pos, "illegal inheritance from sealed class")
	    }
	  }
	  if (!(selfType <:< parent.tpe.typeOfThis)) {
	    System.out.println(context.owner);//debug
	    System.out.println(context.owner.thisSym);//debug
	    error(parent.pos, "illegal inheritance;\n self-type " +
		  selfType + " does not conform to " + parent +
		  "'s selftype " + parent.tpe.typeOfThis);
	    if (settings.explaintypes.value) explainTypes(selfType, parent.tpe.typeOfThis);
	  }
	  if (parents exists (p => p != parent && p.tpe.symbol == psym && !psym.isError))
	    error(parent.pos, "" + psym + " is inherited twice")
	}

      if (!parents.isEmpty) {
        validateParentClass(parents.head, true);
        for (val p <- parents.tail) validateParentClass(p, false);
      }
    }

    def typedClassDef(cdef: ClassDef): Tree = {
      val clazz = cdef.symbol;
      reenterTypeParams(cdef.tparams);
      val tparams1 = List.mapConserve(cdef.tparams)(typedAbsTypeDef);
      val tpt1 = checkNoEscaping.privates(clazz.thisSym, typedType(cdef.tpt));
      val impl1 = newTyper(context.make(cdef.impl, clazz, new Scope()))
        .typedTemplate(cdef.impl);
      copy.ClassDef(cdef, cdef.mods, cdef.name, tparams1, tpt1, impl1) setType NoType
    }

    def typedModuleDef(mdef: ModuleDef): Tree = {
      val clazz = mdef.symbol.moduleClass;
      val impl1 = newTyper(context.make(mdef.impl, clazz, new Scope()))
        .typedTemplate(mdef.impl);
      copy.ModuleDef(mdef, mdef.mods, mdef.name, impl1) setType NoType
    }

    def addGetterSetter(stat: Tree): List[Tree] = stat match {
      case ValDef(mods, name, tpe, rhs) if (mods & PRIVATE) == 0 =>
	val vdef = copy.ValDef(stat, mods | PRIVATE | LOCAL, nme.LOCAL_NAME(name), tpe, rhs);
	val getter: DefDef = {
	  val sym = vdef.symbol;
	  val getter = sym.owner.info.decls.lookup(name).suchThat(.hasFlag(ACCESSOR));
	  assert(getter != NoSymbol, vdef);
	  val result = atPos(vdef.pos)(
	    DefDef(getter, vparamss =>
	      if ((mods & DEFERRED) != 0) EmptyTree else typed(gen.mkRef(sym), EXPRmode, sym.tpe)));
          checkNoEscaping.privates(result.symbol, result.tpt);
          result
	}
	def setter: DefDef = {
	  val sym = vdef.symbol;
	  val setter = sym.owner.info.decls.lookup(nme.SETTER_NAME(name)).suchThat(.hasFlag(ACCESSOR));
          atPos(vdef.pos)(
	    DefDef(setter, vparamss =>
	      if ((mods & DEFERRED) != 0) EmptyTree
	      else typed(Assign(gen.mkRef(getter.symbol), gen.mkRef(vparamss.head.head)))))
	}
	val gs = if ((mods & MUTABLE) != 0) List(getter, setter) else List(getter);
	if ((mods & DEFERRED) != 0) gs else vdef :: gs
      case DocDef(comment, defn) =>
	addGetterSetter(defn) map (stat => DocDef(comment, stat))
      case Attributed(attr, defn) =>
	addGetterSetter(defn) map (stat => Attributed(attr.duplicate, stat))
      case _ =>
	List(stat)
    }

    def typedTemplate(templ: Template): Template = {
      if (templ.symbol == NoSymbol)
	templ setSymbol context.owner.newLocalDummy(templ.pos);
      val parents1 = parentTypes(templ);
      val selfType =
        if (context.owner.isAnonymousClass)
          intersectionType(context.owner.info.parents, context.owner.owner)
        else context.owner.typeOfThis;
      // the following is necessary for templates generated later
      new Namer(context.outer.make(templ, context.owner, context.owner.info.decls))
	.enterSyms(templ.body);
      validateParentClasses(parents1, selfType);
      val body1 = templ.body flatMap addGetterSetter;
      val body2 = typedStats(body1, templ.symbol);
      copy.Template(templ, parents1, body2) setType context.owner.tpe
    }

    def typedValDef(vdef: ValDef): ValDef = {
      val sym = vdef.symbol;
      var tpt1 = checkNoEscaping.privates(sym, typedType(vdef.tpt));
      val rhs1 =
	if (vdef.rhs.isEmpty) vdef.rhs
	else newTyper(context.make(vdef, sym)).typed(vdef.rhs, tpt1.tpe);
      copy.ValDef(vdef, vdef.mods, vdef.name, tpt1, rhs1) setType NoType
    }

    def typedDefDef(ddef: DefDef): DefDef = {
      val meth = ddef.symbol;
      reenterTypeParams(ddef.tparams);
      reenterValueParams(ddef.vparamss);
      val tparams1 = List.mapConserve(ddef.tparams)(typedAbsTypeDef);
      val vparamss1 = List.mapConserve(ddef.vparamss)(vparams1 =>
	List.mapConserve(vparams1)(typedValDef));
      var tpt1 = checkNoEscaping.privates(meth, typedType(ddef.tpt));
      val rhs1 =
	if (ddef.name == nme.CONSTRUCTOR) {
	  if (!meth.hasFlag(SYNTHETIC) &&
	      !(meth.owner.isClass ||
		meth.owner.isModuleClass ||
		meth.owner.isAnonymousClass ||
		meth.owner.isRefinementClass))
	    error(ddef.pos, "constructor definition not allowed here " + meth.owner);//debug
	  context.enclClass.owner.setFlag(INCONSTRUCTOR);
	  val result = typed(ddef.rhs, EXPRmode | INCONSTRmode, UnitClass.tpe);
	  context.enclClass.owner.resetFlag(INCONSTRUCTOR);
	  result
	} else {
	  typed(ddef.rhs, tpt1.tpe);
	}
      copy.DefDef(ddef, ddef.mods, ddef.name, tparams1, vparamss1, tpt1, rhs1) setType NoType
    }

    def typedAbsTypeDef(tdef: AbsTypeDef): AbsTypeDef = {
      val lo1 = checkNoEscaping.privates(tdef.symbol, typedType(tdef.lo));
      val hi1 = checkNoEscaping.privates(tdef.symbol, typedType(tdef.hi));
      copy.AbsTypeDef(tdef, tdef.mods, tdef.name, lo1, hi1) setType NoType
    }

    def typedAliasTypeDef(tdef: AliasTypeDef): AliasTypeDef = {
      reenterTypeParams(tdef.tparams);
      val tparams1 = List.mapConserve(tdef.tparams)(typedAbsTypeDef);
      val rhs1 = checkNoEscaping.privates(tdef.symbol, typedType(tdef.rhs));
      copy.AliasTypeDef(tdef, tdef.mods, tdef.name, tparams1, rhs1) setType NoType
    }

    private def enterLabelDef(stat: Tree): unit = stat match {
      case ldef @ LabelDef(_, _, _) =>
        if (ldef.symbol == NoSymbol)
          ldef.symbol = namer.enterInScope(
            context.owner.newLabel(ldef.pos, ldef.name) setInfo MethodType(List(), UnitClass.tpe));
      case _ =>
    }

    def typedLabelDef(ldef: LabelDef): LabelDef = {
      val restpe = ldef.symbol.tpe.resultType;
      val rhs1 = typed(ldef.rhs, restpe);
      ldef.params foreach (param => param.tpe = param.symbol.tpe);
      copy.LabelDef(ldef, ldef.name, ldef.params, rhs1) setType restpe
    }

    def typedBlock(block: Block, mode: int, pt: Type): Block = {
      namer.enterSyms(block.stats);
      block.stats foreach enterLabelDef;
      val stats1 =
        if ((mode & INCONSTRmode) != 0) {
          val constrCall = typed(block.stats.head, mode, WildcardType);
          context.enclClass.owner.resetFlag(INCONSTRUCTOR);
          constrCall :: typedStats(block.stats.tail, context.owner);
        } else {
	  typedStats(block.stats, context.owner)
        }
      val expr1 = typed(block.expr, mode & ~(FUNmode | QUALmode), pt);
      val block1 = copy.Block(block, stats1, expr1)
        setType (if (treeInfo.isPureExpr(block)) expr1.tpe else expr1.tpe.deconst);
      if (block1.tpe.symbol.isAnonymousClass)
	block1 setType intersectionType(block1.tpe.parents, block1.tpe.symbol.owner);
      if (isFullyDefined(pt)) block1 else checkNoEscaping.locals(context.scope, block1)
    }

    def typedCase(cdef: CaseDef, pattpe: Type, pt: Type): CaseDef = {
      val pat1: Tree = typedPattern(cdef.pat, pattpe);
      val guard1: Tree = if (cdef.guard == EmptyTree) EmptyTree
	                 else typed(cdef.guard, BooleanClass.tpe);
      val body1: Tree = typed(cdef.body, pt);
      copy.CaseDef(cdef, pat1, guard1, body1) setType body1.tpe
    }

    /*  Transform a function node (x_1,...,x_n) => body of type FunctionN[T_1, .., T_N, R] to
     *
     *    class $anon() extends Object() with FunctionN[T_1, .., T_N, R] with ScalaObject {
     *      def apply(x_1: T_1, ..., x_N: T_n): R = body
     *    }
     *    new $anon()
     *
     *  transform a function node (x => body) of type PartialFunction[T, R] where
     *    body = x match { case P_i if G_i => E_i }_i=1..n
     *  to:
     *
     *    class $anon() extends Object() with PartialFunction[T, R] with ScalaObject {
     *      def apply(x: T): R = body;
     *      def isDefinedAt(x: T): boolean = x match {
     *        case P_1 if G_1 => true
     *        ...
     *        case P_n if G_n => true
     *        case _ => false
     *      }
     *    }
     *    new $anon()
     *
     *  However, if one of the patterns P_i if G_i is a default pattern, generate instead
     *
     *      def isDefinedAt(x: T): boolean = true
     */
    def typedFunction(fun: Function, mode: int, pt: Type): Tree = {
      val Triple(clazz, argpts, respt) =
	if (isFunctionType(pt)
	    ||
            pt.symbol == PartialFunctionClass &&
            fun.vparams.length == 1 && fun.body.isInstanceOf[Match])
          Triple(pt.symbol, pt.typeArgs.init, pt.typeArgs.last)
        else
          Triple(FunctionClass(fun.vparams.length), fun.vparams map (x => NoType), WildcardType);
      val vparamSyms = List.map2(fun.vparams, argpts) { (vparam, argpt) =>
        if (vparam.tpt.isEmpty)
          vparam.tpt.tpe =
            if (argpt == NoType) { error(vparam.pos, "missing parameter type"); ErrorType }
	    else argpt;
        namer.enterSym(vparam);
	vparam.symbol
      }
      val vparams = List.mapConserve(fun.vparams)(typedValDef);
      val body = typed(fun.body, respt);
      val formals = vparamSyms map (.tpe);
      val funtpe = typeRef(clazz.tpe.prefix, clazz, formals ::: List(body.tpe));
      val anonClass = context.owner.newAnonymousFunctionClass(fun.pos) setFlag (FINAL | SYNTHETIC);
      anonClass setInfo ClassInfoType(
	List(ObjectClass.tpe, funtpe, ScalaObjectClass.tpe), new Scope(), anonClass);
      val applyMethod = anonClass.newMethod(fun.pos, nme.apply)
	setFlag FINAL setInfo MethodType(formals, body.tpe);
      anonClass.info.decls enter applyMethod;
      for (val vparam <- vparamSyms) vparam.owner = applyMethod;
      var members = List(
	DefDef(FINAL, nme.apply, List(), List(vparams), TypeTree(body.tpe), body)
	  setSymbol applyMethod);
      if (pt.symbol == PartialFunctionClass) {
	val isDefinedAtMethod = anonClass.newMethod(fun.pos, nme.isDefinedAt)
	  setFlag FINAL setInfo MethodType(formals, BooleanClass.tpe);
	anonClass.info.decls enter isDefinedAtMethod;
	def idbody(idparam: Symbol) = body match {
	  case Match(_, cases) =>
	    val substParam = new TreeSymSubstituter(List(vparams.head.symbol), List(idparam));
	    def transformCase(cdef: CaseDef): CaseDef =
	      CaseDef(substParam(cdef.pat.duplicate),
		      substParam(cdef.guard.duplicate),
		      Literal(true));
	    def isDefaultCase(cdef: CaseDef) = cdef match {
	      case CaseDef(Ident(nme.WILDCARD), EmptyTree, _) => true
	      case CaseDef(Bind(_, Ident(nme.WILDCARD)), EmptyTree, _) => true
	      case _ => false
	    }
	    if (cases exists isDefaultCase) Literal(true)
	    else Match(
	      Ident(idparam),
	      (cases map transformCase) :::
		List(CaseDef(Ident(nme.WILDCARD), EmptyTree, Literal(false))))
	}
	members = DefDef(isDefinedAtMethod, vparamss => idbody(vparamss.head.head)) :: members;
      }
      typed(
	atPos(fun.pos)(
	  Block(
	    List(ClassDef(anonClass, List(List()), List(List()), members)),
	    Typed(
	      New(TypeTree(anonClass.tpe), List(List())),
	      TypeTree(funtpe)))))
    }

    def typedRefinement(stats: List[Tree]): List[Tree] = {
      namer.enterSyms(stats);
      for (val stat <- stats) stat.symbol setFlag OVERRIDE;
      typedStats(stats, NoSymbol);
    }

    def typedStats(stats: List[Tree], exprOwner: Symbol): List[Tree] =
      List.mapConserve(stats) { stat =>
        if (context.owner.isRefinementClass && !treeInfo.isDeclaration(stat))
	  errorTree(stat, "only declarations allowed here");
	stat match {
	  case imp @ Import(_, _) =>
	    context = context.makeNewImport(imp);
	    EmptyTree
	  case _ =>
	    (if (exprOwner != context.owner && (!stat.isDef || stat.isInstanceOf[LabelDef]))
	      newTyper(context.make(stat, exprOwner)) else this).typed(stat)
	}
      }

    private def typed1(tree: Tree, mode: int, pt: Type): Tree = {

      def funmode = mode & stickyModes | FUNmode | POLYmode;

      def ptOrLub(tps: List[Type]) = if (isFullyDefined(pt)) pt else lub(tps);

      def typedCases(cases: List[CaseDef], pattp: Type): List[CaseDef] = {
        List.mapConserve(cases)(cdef =>
	  newTyper(context.makeNewScope(tree, context.owner)).typedCase(cdef, pattp, pt))
      }

      def typedTypeApply(fun: Tree, args: List[Tree]): Tree = fun.tpe match {
	case OverloadedType(pre, alts) =>
          inferPolyAlternatives(fun, args.length);
          typedTypeApply(fun, args)
        case PolyType(tparams, restpe) if (tparams.length != 0) =>
          if (tparams.length == args.length) {
            val targs = args map (.tpe);
            checkBounds(tree.pos, tparams, targs, "");
	    if (settings.debug.value) log("type app " + tparams + " => " + targs + " = " + restpe.subst(tparams, targs));//debug
	    copy.TypeApply(tree, fun, args) setType restpe.subst(tparams, targs);
          } else {
            errorTree(tree, "wrong number of type parameters for " + treeSymTypeMsg(fun))
          }
	case ErrorType =>
	  setError(tree)
	case _ =>
	  System.out.println(fun.toString() + " " + args);//debug
          errorTree(tree, treeSymTypeMsg(fun) + " does not take type parameters.");
        }

      def typedApply(fun: Tree, args: List[Tree]): Tree = fun.tpe match {
        case OverloadedType(pre, alts) =>
          val args1 = List.mapConserve(args)(arg =>
            typed(arg, mode & stickyModes, WildcardType));
          inferMethodAlternative(fun, context.undetparams, args1 map (.tpe.deconst), pt);
          typedApply(adapt(fun, funmode, WildcardType), args1);
        case MethodType(formals0, restpe) =>
          val formals = formalTypes(formals0, args.length);
          if (formals.length != args.length) {
	    System.out.println("" + formals.length + " " + args.length);
            errorTree(tree, "wrong number of arguments for " + treeSymTypeMsg(fun))
          } else {
            val tparams = context.undetparams;
            context.undetparams = List();
            if (tparams.isEmpty) {
              val args1 = List.map2(args, formals) ((arg, formal) =>
                typed(arg, mode & stickyModes, formal));
	      def ifPatternSkipFormals(tp: Type) = tp match {
		case MethodType(_, rtp) if ((mode & PATTERNmode) != 0) => rtp
		case _ => tp
	      }
              constfold(copy.Apply(tree, fun, args1).setType(ifPatternSkipFormals(restpe)));
	    } else {
	      assert((mode & PATTERNmode) == 0); // this case cannot arise for patterns
              val lenientTargs = protoTypeArgs(tparams, formals, restpe, pt);
              val strictTargs = List.map2(lenientTargs, tparams)((targ, tparam) =>
                if (targ == WildcardType) tparam.tpe else targ);
              def typedArg(tree: Tree, formal: Type): Tree = {
	        val lenientPt = formal.subst(tparams, lenientTargs);
	        val tree1 = typed(tree, mode & stickyModes | POLYmode, lenientPt);
	        val argtparams = context.undetparams;
	        context.undetparams = List();
	        if (!argtparams.isEmpty) {
	          val strictPt = formal.subst(tparams, strictTargs);
	          inferArgumentInstance(tree1, argtparams, strictPt, lenientPt);
	        }
	        tree1
              }
              val args1 = List.map2(args, formals)(typedArg);
              if (args1 exists (.tpe.isError)) setError(tree)
              else {
                if (settings.debug.value) log("infer method inst " + fun + ", tparams = " + tparams + ", args = " + args1.map(.tpe) + ", pt = " + pt + ", lobounds = " + tparams.map(.tpe.bounds.lo));//debug
                val undetparams = inferMethodInstance(fun, tparams, args1, pt);
                val result = typedApply(fun, args1);
                context.undetparams = undetparams;
                result
              }
            }
          }
        case ErrorType =>
          setError(tree)
        case _ =>
          throw new Error("Matcherror at " + phase);//debug
      }

      /** The qualifying class of a this or super with prefix `qual' */
      def qualifyingClass(qual: Name): Symbol = {
        if (qual == nme.EMPTY.toTypeName) {
          val clazz = context.enclClass.owner;
          if (!clazz.isPackageClass) clazz
          else {
            error(tree.pos, "" + tree + " can be used only in a class, object, or template");
            NoSymbol
          }
        } else {
          var c = context.enclClass;
          while (c != NoContext && c.owner.name != qual) c = c.outer.enclClass;
          if (c != NoContext) c.owner
          else {
            error(tree.pos, "" + qual + " is not an enclosing class");
	    NoSymbol
          }
        }
      }

      /** Attribute a selection where `tree' is `qual.name'.
       *  `qual' is already attributed.
       */
      def typedSelect(qual: Tree, name: Name): Tree = {
	val sym =
	  if (tree.symbol != NoSymbol) {
	    val alts = qual.tpe.member(name).alternatives;
	    if (alts.filter(tree.symbol.==).isEmpty)
	      assert(false, "symbol " + tree.symbol + " not in " + alts);
	    tree.symbol;
	  } else {
	    qual.tpe.member(name)
	  }
	if (sym == NoSymbol && qual.isTerm && (qual.symbol == null || qual.symbol.isValue)) {
	  val coercion = inferView(qual.pos, qual.tpe, name, true);
	  if (coercion != EmptyTree)
	    return typed(
	      copy.Select(tree, Apply(coercion, List(qual)) setPos qual.pos, name), mode, pt)
	}
        if (sym.info == NoType) {
          if (settings.debug.value) log("qual = " + qual + ":" + qual.tpe + "\nSymbol=" + qual.tpe.symbol + "\nsymbol-info = " + qual.tpe.symbol.info + "\nscope-id = " + qual.tpe.symbol.info.decls.hashCode() + "\nmembers = " + qual.tpe.members + "\nfound = " + sym);
          errorTree(tree,
	    decode(name) + " is not a member of " + qual.tpe.widen +
	    (if (Position.line(tree.pos) > Position.line(qual.pos))
	      "\npossible cause: maybe a semicolon is missing before `" + name + "'?" else ""))
        } else {
	  val tree1 = tree match {
	    case Select(_, _) => copy.Select(tree, qual, name)
	    case SelectFromTypeTree(_, _) => copy.SelectFromTypeTree(tree, qual, name);
	  }
	  stabilize(checkAccessible(tree1, sym, qual.tpe, qual), qual.tpe, mode, pt);
	}
      }

      /** Attribute an identifier consisting of a simple name or an outer reference.
       *  @param tree      The tree representing the identifier.
       *  @param name      The name of the identifier.
       *  Transformations: (1) Prefix class members with this.
       *                   (2) Change imported symbols to selections
       */
      def typedIdent(name: Name): Tree = {
	def ambiguousError(msg: String) =
	  error(tree.pos, "reference to " + name + " is ambiguous;\n" + msg);

	var defSym: Symbol = tree.symbol;   // the directly found symbol
	var pre: Type = NoPrefix;        // the prefix type of defSym, if a class member
	var qual: Tree = EmptyTree;   // the qualififier tree if transformed tree is a select

	if (defSym == NoSymbol) {
	  var defEntry: ScopeEntry = null; // the scope entry of defSym, if defined in a local scope

	  var cx = context;
	  while (defSym == NoSymbol && cx != NoContext) {
	    pre = cx.enclClass.owner.thisType;
	    defEntry = cx.scope.lookupEntry(name);
	    if (defEntry != null)
	      defSym = defEntry.sym
	    else {
	      cx = cx.enclClass;
	      defSym = pre.member(name) filter (sym => context.isAccessible(sym, pre, false));
	      if (defSym == NoSymbol) cx = cx.outer;
	    }
	  }
	  val symDepth = if (defEntry == null) cx.depth
			 else cx.depth - (cx.scope.nestingLevel - defEntry.owner.nestingLevel);
	  var impSym: Symbol = NoSymbol;      // the imported symbol
	  var imports = context.imports;      // impSym != NoSymbol => it is imported from imports.head
	  while (impSym == NoSymbol && !imports.isEmpty && imports.head.depth > symDepth) {
	    impSym = imports.head.importedSymbol(name);
	    if (impSym == NoSymbol) imports = imports.tail;
	  }

	  // detect ambiguous definition/import,
	  // update `defSym' to be the final resolved symbol,
	  // update `pre' to be `sym's prefix type in case it is an imported member,
	  // and compute value of:

	  // imported symbols take precedence over external package-owned symbols (hack?)
	  if (defSym.tpe != NoType && impSym.tpe != NoType && defSym.isExternal && defSym.owner.isPackageClass)
	    defSym = NoSymbol;

	  if (defSym.tpe != NoType) {
	    if (impSym.tpe != NoType)
	      ambiguousError(
		"it is both defined in " + defSym.owner +
		" and imported subsequently by \n" + imports.head);
	    else if (!defSym.owner.isClass || defSym.owner.isPackageClass || defSym.isTypeParameter)
	      pre = NoPrefix
	    else
	      qual = atPos(tree.pos)(gen.mkQualifier(pre));
	  } else {
	    if (impSym.tpe != NoType) {
	      var impSym1 = NoSymbol;
	      var imports1 = imports.tail;
	      def ambiguousImportError = ambiguousError(
		"it is imported twice in the same scope by\n" + imports.head +  "\nand " + imports1.head);
	      while (!imports1.isEmpty && imports1.head.depth == imports.head.depth) {
		var impSym1 = imports1.head.importedSymbol(name);
		if (impSym1 != NoSymbol) {
		  if (imports1.head.isExplicitImport(name)) {
		    if (imports.head.isExplicitImport(name)) ambiguousImportError;
		    impSym = impSym1;
		    imports = imports1;
		  } else if (!imports.head.isExplicitImport(name)) ambiguousImportError
		}
		imports1 = imports1.tail;
	      }
	      defSym = impSym;
	      qual = imports.head.qual;
	      pre = qual.tpe;
	    } else {
              if (settings.debug.value) {
	        log(context);//debug
	        log(context.imports);//debug
              }
	      error(tree.pos, "not found: " + decode(name));
	      defSym = context.owner.newErrorSymbol(name);
	    }
	  }
	}
        if (defSym.owner.isPackageClass) pre = defSym.owner.thisType;
	val tree1 = if (qual == EmptyTree) tree
                    else atPos(tree.pos)(Select(qual, name));
		      // atPos necessary because qualifier might come from startContext
	stabilize(checkAccessible(tree1, defSym, pre, qual), pre, mode, pt)
      }

      // begin typed1
      val sym: Symbol = tree.symbol;
      if (sym != null) sym.initialize;
      //if (settings.debug.value && tree.isDef) log("typing definition of " + sym);//DEBUG
      tree match {
        case PackageDef(name, stats) =>
          val stats1 = newTyper(context.make(tree, sym.moduleClass, sym.info.decls))
            .typedStats(stats, NoSymbol);
          copy.PackageDef(tree, name, stats1) setType NoType

        case cdef @ ClassDef(_, _, _, _, _) =>
          newTyper(context.makeNewScope(tree, sym)).typedClassDef(cdef)

        case mdef @ ModuleDef(_, _, _) =>
          typedModuleDef(mdef)

        case vdef @ ValDef(_, _, _, _) =>
          typedValDef(vdef)

        case ddef @ DefDef(_, _, _, _, _, _) =>
          newTyper(context.makeNewScope(tree, sym)).typedDefDef(ddef)

        case tdef @ AbsTypeDef(_, _, _, _) =>
          newTyper(context.makeNewScope(tree, sym)).typedAbsTypeDef(tdef)

        case tdef @ AliasTypeDef(_, _, _, _) =>
          newTyper(context.makeNewScope(tree, sym)).typedAliasTypeDef(tdef)

        case ldef @ LabelDef(_, _, _) =>
          var lsym = ldef.symbol;
          var typer1 = this;
          if (lsym == NoSymbol) { // labeldef is part of template
            typer1 = newTyper(context.makeNewScope(tree, context.owner));
            typer1.enterLabelDef(ldef);
          }
          typer1.typedLabelDef(ldef)

        case Attributed(attr, defn) =>
          val attr1 = typed(attr, AttributeClass.tpe);
          val defn1 = typed(defn, mode, pt);
          val existing = attributes.get(defn1.symbol) match {
            case None => List()
            case Some(attrs) => attrs
          }
          attributes(defn1.symbol) = attrInfo(attr1) :: existing;
          defn1

        case DocDef(comment, defn) =>
          typed(defn, mode, pt);

        case block @ Block(_, _) =>
          newTyper(context.makeNewScope(tree, context.owner))
            .typedBlock(block, mode, pt)

        case Sequence(elems) =>
	  val elems1 = List.mapConserve(elems)(elem => typed(elem, mode, pt));
          copy.Sequence(tree, elems1) setType pt

        case Alternative(alts) =>
	  val alts1 = List.mapConserve(alts)(alt => typed(alt, mode, pt));
          copy.Alternative(tree, alts1) setType pt

        case Star(elem) =>
	  val elem1 = typed(elem, mode, pt);
          copy.Star(tree, elem1) setType pt

        case Bind(name, body) =>
          var vble = tree.symbol;
          if (vble == NoSymbol) vble = context.owner.newValue(tree.pos, name);
          val body1 = typed(body, mode, pt);
          vble.setInfo(if (treeInfo.isSequenceValued(body)) seqType(body1.tpe) else body1.tpe);
          if (vble.name != nme.WILDCARD) namer.enterInScope(vble);
          copy.Bind(tree, name, body1) setSymbol vble setType pt

        case ArrayValue(elemtpt, elems) =>
	  val elemtpt1 = typedType(elemtpt);
	  val elems1 = List.mapConserve(elems)(elem => typed(elem, mode, elemtpt1.tpe));
          copy.ArrayValue(tree, elemtpt1, elems1)
            setType (if (isFullyDefined(pt)) pt
                     else appliedType(ArrayClass.typeConstructor, List(elemtpt1.tpe)))

        case fun @ Function(_, _) =>
          newTyper(context.makeNewScope(tree, context.owner)).typedFunction(fun, mode, pt)

        case Assign(lhs, rhs) =>
          def isGetter(sym: Symbol) = sym.info match {
            case PolyType(List(), _) => sym.owner.isClass && !sym.isStable
            case _ => false
          }
          val lhs1 = typed(lhs);
          val varsym = lhs1.symbol;
          if (varsym != null && isGetter(varsym)) {
            lhs1 match {
              case Select(qual, name) =>
                typed(
		  Apply(
		    Select(qual, nme.SETTER_NAME(name)) setPos lhs.pos,
		    List(rhs)) setPos tree.pos, mode, pt)
            }
          } else if (varsym != null && varsym.isVariable) {
            val rhs1 = typed(rhs, lhs1.tpe);
            copy.Assign(tree, lhs1, rhs1) setType UnitClass.tpe;
          } else {
            System.out.println("" + lhs1 + " " + varsym + " " + flagsToString(varsym.flags));//debug
            if (!lhs1.tpe.isError) error(tree.pos, "assignment to non-variable ");
            setError(tree)
          }

        case If(cond, thenp, elsep) =>
          val cond1 = typed(cond, BooleanClass.tpe);
          if (elsep.isEmpty) {
            val thenp1 = typed(thenp, UnitClass.tpe);
            copy.If(tree, cond1, thenp1, elsep) setType UnitClass.tpe
          } else {
            val thenp1 = typed(thenp, pt);
            val elsep1 = typed(elsep, pt);
            copy.If(tree, cond1, thenp1, elsep1) setType ptOrLub(List(thenp1.tpe, elsep1.tpe));
          }

        case Match(selector, cases) =>
          val selector1 = typed(selector);
          val cases1 = typedCases(cases, selector1.tpe);
          copy.Match(tree, selector1, cases1) setType ptOrLub(cases1 map (.tpe))

        case Return(expr) =>
          val enclFun = if (tree.symbol != NoSymbol) tree.symbol else context.owner.enclMethod;
          if (!enclFun.isMethod || enclFun.isConstructor)
            errorTree(tree, "return outside method definition")
          else if (!context.owner.hasFlag(INITIALIZED))
            errorTree(tree, "method " + context.owner + " has return statement; needs result type")
          else {
            val expr1: Tree = typed(expr, enclFun.tpe.resultType);
            copy.Return(tree, expr1) setSymbol enclFun setType AllClass.tpe;
          }

        case Try(block, catches, finalizer) =>
          val block1 = typed(block, pt);
          val catches1 = typedCases(catches, ThrowableClass.tpe);
          val finalizer1 = if (finalizer.isEmpty) finalizer
                           else typed(finalizer, UnitClass.tpe);
          copy.Try(tree, block1, catches1, finalizer1)
            setType ptOrLub(block1.tpe :: (catches1 map (.tpe)))

        case Throw(expr) =>
          val expr1 = typed(expr, ThrowableClass.tpe);
          copy.Throw(tree, expr1) setType AllClass.tpe

        case New(tpt: Tree) =>
          var tpt1 = typedTypeConstructor(tpt);
          if (tpt1.hasSymbol && !tpt1.symbol.typeParams.isEmpty) {
	    context.undetparams = cloneSymbols(tpt1.symbol.unsafeTypeParams);
            tpt1 = TypeTree()
              setPos tpt1.pos
              setType appliedType(tpt1.tpe, context.undetparams map (.tpe));
          }
          copy.New(tree, tpt1).setType(tpt1.tpe)

        case Typed(expr, tpt @ Ident(name)) if (name == nme.WILDCARD_STAR.toTypeName) =>
          val expr1 = typed(expr, mode & stickyModes, seqType(pt));
          expr1.tpe.baseType(SeqClass) match {
            case TypeRef(_, _, List(elemtp)) =>
              copy.Typed(tree, expr1, tpt setType elemtp) setType elemtp
            case _ =>
              setError(tree)
          }
        case Typed(expr, tpt) =>
          val tpt1 = typedType(tpt);
          val expr1 = typed(expr, mode & stickyModes, tpt1.tpe);
          copy.Typed(tree, expr1, tpt1) setType tpt1.tpe

        case TypeApply(fun, args) =>
	  val args1 = List.mapConserve(args)(typedType);
	  // do args first in order to maintain conext.undetparams on the function side.
          typedTypeApply(typed(fun, funmode | TAPPmode, WildcardType), args1)

        case Apply(fun, args) =>
	  val stableApplication =
	    fun.symbol != null && fun.symbol.tpe.isInstanceOf[MethodType] && fun.symbol.isStable;
	  if (stableApplication && (mode & PATTERNmode) != 0) {
	    // treat stable function applications f() as expressions.
	    typed1(tree, mode & ~PATTERNmode | EXPRmode, pt)
	  } else {
            val funpt = if ((mode & PATTERNmode) != 0) pt else WildcardType;
            var fun1 = typed(fun, funmode, funpt);
	    if (stableApplication) fun1 = stabilizeFun(fun1, mode, pt);
            // if function is overloaded, filter all alternatives that match
	    // number of arguments and expected result type.
	    // if (settings.debug.value) log("trans app " + fun1 + ":" + fun1.symbol + ":" + fun1.tpe + " " + args);//DEBUG
	    if (fun1.hasSymbol && fun1.symbol.hasFlag(OVERLOADED)) {
	      val argtypes = args map (arg => AllClass.tpe);
	      val pre = fun1.symbol.tpe.prefix;
              val sym = fun1.symbol filter (alt =>
		isApplicable(context.undetparams, pre.memberType(alt), argtypes, pt));
              if (sym != NoSymbol)
		fun1 = adapt(fun1 setSymbol sym setType pre.memberType(sym), funmode, WildcardType)
            }
	    appcnt = appcnt + 1;
            typedApply(fun1, args)
	  }

        case Super(qual, mix) =>
          val clazz = if (tree.symbol != NoSymbol) tree.symbol else qualifyingClass(qual);
          if (clazz == NoSymbol) setError(tree)
          else {
	    val owntype =
	      if (mix == nme.EMPTY.toTypeName)
                if ((mode & SUPERCONSTRmode) != 0) clazz.info.parents.head
                else intersectionType(clazz.info.parents)
              else {
                val ps = clazz.info.parents dropWhile (p => p.symbol.name != mix);
                if (ps.isEmpty) {
                  System.out.println(clazz.info.parents map (.symbol.name));//debug
                  error(tree.pos, "" + mix + " does not name a base class of " + clazz);
                  ErrorType
                } else ps.head
              }
	    tree setSymbol clazz setType SuperType(clazz.thisType, owntype)
	  }

        case This(qual) =>
          val clazz = if (tree.symbol != NoSymbol) tree.symbol else qualifyingClass(qual);
          if (clazz == NoSymbol) setError(tree)
          else {
	    val owntype = if (pt.isStable || (mode & QUALmode) != 0) clazz.thisType
			  else clazz.typeOfThis;
            tree setSymbol clazz setType owntype
	  }

        case Select(qual @ Super(_, _), nme.CONSTRUCTOR) =>
          val qual1 = typed(qual, EXPRmode | QUALmode | POLYmode | SUPERCONSTRmode, WildcardType);
          // the qualifier type of a supercall constructor is its first parent class
	  typedSelect(qual1, nme.CONSTRUCTOR);

        case Select(qual, name) =>
	  selcnt = selcnt + 1;
	  assert (name != nme.CONSTRUCTOR || !qual.isInstanceOf[Super], tree);//debug
          var qual1 = typedQualifier(qual);
          if (name.isTypeName) qual1 = checkStable(qual1);
          typedSelect(qual1, name);

        case Ident(name) =>
	  idcnt = idcnt + 1;
	  if (name == nme.WILDCARD && (mode & (PATTERNmode | FUNmode)) == PATTERNmode)
	    tree setType pt
	  else
	    typedIdent(name);

	// todo: try with case Literal(Constant(()))
        case Literal(value) =>
          tree setType (if (value.tag == UnitTag) UnitClass.tpe else ConstantType(value))

        case SingletonTypeTree(ref) =>
          val ref1 = checkStable(typed(ref, EXPRmode | QUALmode, AnyRefClass.tpe));
          tree setType ref1.tpe.resultType;

        case SelectFromTypeTree(qual, selector) =>
          tree setType typedSelect(typedType(qual), selector).tpe

        case CompoundTypeTree(templ: Template) =>
          tree setType {
            val parents1 = List.mapConserve(templ.parents)(typedType);
            if (parents1 exists (.tpe.isError)) ErrorType
            else {
              val decls = new Scope();
              val self = refinedType(parents1 map (.tpe), context.enclClass.owner, decls);
              newTyper(context.make(tree, self.symbol, new Scope())).typedRefinement(templ.body);
              self
            }
          }

        case AppliedTypeTree(tpt, args) =>
          val tpt1 = typed(tpt, mode | FUNmode | TAPPmode, WildcardType);
          val tparams = tpt1.tpe.symbol.typeParams;
          val args1 = List.mapConserve(args)(typedType);
          if (tpt1.tpe.isError)
            setError(tree)
          else if (tparams.length == args1.length)
            TypeTree() setPos tree.pos setType appliedType(tpt1.tpe, args1 map (.tpe))
          else if (tparams.length == 0)
            errorTree(tree, "" + tpt1.tpe + " does not take type parameters")
          else
            errorTree(tree, "wrong number of type arguments for " + tpt1.tpe)
      }
    }

    def typed(tree: Tree, mode: int, pt: Type): Tree =
      try {
        if (settings.debug.value) {
          assert(pt != null, tree);//debug
	  //System.out.println("typing " + tree);//debug
	}
        val tree1 = if (tree.tpe != null) tree else typed1(tree, mode, pt);
	//System.out.println("typed " + tree1 + ":" + tree1.tpe);//DEBUG
        val result = if (tree1.isEmpty) tree1 else adapt(tree1, mode, pt);
	//System.out.println("adpated " + tree1 + ":" + tree1.tpe + " to " + pt);//DEBUG
	result
      } catch {
        case ex: TypeError =>
	  reportTypeError(tree.pos, ex);
	  setError(tree)
	case ex: Throwable =>
	  if (settings.debug.value)
	    System.out.println("exception when typing " + tree + ", pt = " + pt);
	  throw(ex)
      }

    def atOwner(owner: Symbol): Typer =
      new Typer(context.make(context.tree, owner));

    def atOwner(tree: Tree, owner: Symbol): Typer =
      new Typer(context.make(tree, owner));

    /** Types expression or definition `tree' */
    def typed(tree: Tree): Tree =
      typed(tree, EXPRmode, WildcardType);

    /** Types expression `tree' with given prototype `pt' */
    def typed(tree: Tree, pt: Type): Tree =
      typed(tree, EXPRmode, pt);

    /** Types qualifier `tree' of a select node. E.g. is tree occurs in acontext like `tree.m'. */
    def typedQualifier(tree: Tree): Tree =
      typed(tree, EXPRmode | QUALmode | POLYmode, WildcardType);

    /** Types function part of an application */
    def typedOperator(tree: Tree): Tree =
      typed(tree, EXPRmode | FUNmode | POLYmode | TAPPmode, WildcardType);

    /** Types a pattern with prototype `pt' */
    def typedPattern(tree: Tree, pt: Type): Tree =
      typed(tree, PATTERNmode, pt);

    /** Types a (fully parameterized) type tree */
    def typedType(tree: Tree): Tree =
      typed(tree, TYPEmode, WildcardType);

    /** Types a type or type constructor tree */
    def typedTypeConstructor(tree: Tree): Tree =
      typed(tree, TYPEmode | FUNmode, WildcardType);

    /* -- Views --------------------------------------------------------------- */

    private def depoly(tp: Type): Type = tp match {
      case PolyType(tparams, restpe) => restpe.subst(tparams, tparams map (t => WildcardType))
      case _ => tp
    }

    private def typedImplicit(pos: int, info: ImplicitInfo, pt: Type, local: boolean): Tree =
      if (isCompatible(depoly(info.tpe), pt)) {
	implcnt = implcnt + 1;
	var tree: Tree = EmptyTree;
        def fail(reason: String): Tree = {
          if (settings.debug.value)
            log(tree.toString() + " is not a valid implicit value because:\n" + reason);
          EmptyTree
        }
	try {
          tree = Ident(info.name) setPos pos;
          if (!local) tree setSymbol info.sym;
	  tree = typed1(tree, EXPRmode, pt);
	  if (settings.debug.value) log("typed implicit " + tree + ":" + tree.tpe + ", pt = " + pt);//debug
	  val tree1 = adapt(tree, EXPRmode, pt);
	  if (settings.debug.value) log("adapted implicit " + tree.symbol + ":" + info.sym);//debug
	  if (info.sym == tree.symbol) tree1
          else fail("syms differ: " + tree.symbol + " " + info.sym)
	} catch {
	  case ex: TypeError => fail(ex.getMessage())
	}
      } else EmptyTree;

    private def inferImplicit(pos: int, pt: Type, isView: boolean, reportAmbiguous: boolean): Tree = {

      def isBetter(sym1: Symbol, tpe1: Type, sym2: Symbol, tpe2: Type): boolean =
        sym2.isError ||
	(sym1.owner != sym2.owner) && (sym1.owner isSubClass sym2.owner) && (tpe1 matches tpe2);

      val tc = newTyper(context.makeImplicit(reportAmbiguous));

      def searchImplicit(implicitInfoss: List[List[ImplicitInfo]], local: boolean): Tree = {
	var iss = implicitInfoss;
	var tree: Tree = EmptyTree;
	while (tree == EmptyTree && !iss.isEmpty) {
	  var is = iss.head;
	  iss = iss.tail;
	  while (!is.isEmpty) {
	    tree = tc.typedImplicit(pos, is.head, pt, local);
            if (settings.debug.value) log("tested " + is.head.sym + is.head.sym.locationString + ":" + is.head.tpe + "=" + tree);//debug
	    val is0 = is;
	    is = is.tail;
	    if (tree != EmptyTree) {
	      while (!is.isEmpty) {
		val tree1 = tc.typedImplicit(pos, is.head, pt, local);
		if (tree1 != EmptyTree) {
		  if (isBetter(is.head.sym, tree1.tpe, is0.head.sym, tree.tpe))
		    tree = tree1
		  else if (!isBetter(is0.head.sym, tree.tpe, is.head.sym, tree1.tpe))
		    error(
		      pos,
		      "ambiguous implicit value:\n" +
		      " both " + is0.head.sym + is0.head.sym.locationString + " of type " + tree.tpe +
		      "\n and" + is.head.sym + is.head.sym.locationString + " of type " + tree1.tpe +
		      (if (isView)
			"\n are possible conversion functions from " +
		       pt.typeArgs(0) + " to " + pt.typeArgs(1)
		       else
			 "\n match expected type " + pt));
		}
		is = is.tail
	      }
	    }
	  }
	}
	tree
      }

      def implicitsOfType(tp: Type): List[List[ImplicitInfo]] = {
	val tp1 = if (isFunctionType(tp)) intersectionType(tp.typeArgs.reverse) else tp;
	tp1.baseClasses map implicitsOfClass;
      }

      def implicitsOfClass(clazz: Symbol): List[ImplicitInfo] =
        clazz.initialize.linkedModule.moduleClass.info.decls.toList.filter(.hasFlag(IMPLICIT)) map
	  (sym => ImplicitInfo(sym.name, clazz.linkedModule.tpe.memberType(sym), sym));

      var tree = searchImplicit(context.implicitss, true);
      if (tree == EmptyTree) tree = searchImplicit(implicitsOfType(pt.widen), false);
      tree
    }

    def applyImplicitArgs(tree: Tree): Tree = tree.tpe match {
      case MethodType(formals, _) =>
        def implicitArg(pt: Type) = {
          val arg = inferImplicit(tree.pos, pt, false, true);
          if (arg != EmptyTree) arg
          else errorTree(tree, "no implicit argument matching parameter type " + pt + " was found.")
        }
      Apply(tree, formals map implicitArg) setPos tree.pos
    }
  }
}

