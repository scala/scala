/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

import java.lang.Object;

import scalac.{Global => scalac_Global}
import scalac.typechecker.{Infer => scalac_Infer}
import scalac.ApplicationError;
import scalac.util._;
import scalac.ast._;
import scalac.symtab._;
import scala.collection.mutable.HashMap;
import scala.tools.util.Position;

import scala.tools.scalac.util.NewArray;

package scala.tools.scalac.typechecker {

class Infer(global: scalac_Global, gen: TreeGen, make: TreeFactory) extends scalac_Infer {

  import Modifiers._, Kinds._;

  val definitions: Definitions = global.definitions;
  val substituter: Substituter = new Substituter(global, gen);

  def this(trans: Transformer) = this(trans.global, trans.gen, trans.make);

// Context accessor and error function, overridable */

  def getContext: Context = Context.NONE;

  def error(pos: int, msg: String): Tree =
    throw new Type$Error(msg);


// Error messages -------------------------------------------------------------

  def applyErrorMsg(msg1: String, fn: Tree, msg2: String, argtypes: Array[Type], pt: Type): String =
    msg1 + toString(fn.symbol(), fn.getType()) + msg2 +
    ArrayApply.toString(argtypes.asInstanceOf[Array[Object]], "(", ",", ")") +
    (if (pt == Type.AnyType) "" else " with expected result type " + pt);

  def typeErrorMsg(msg: String, found: Type, req: Type): String =
    msg + ";\n found   : " + found.toLongString() + "\n required: " + req;

  def overloadResolveErrorMsg(sym1: Symbol, tpe1: Type, sym2: Symbol, tpe2: Type): String =
    "ambiguous reference to overloaded definition,\n" +
    "both " + sym1 + ": " + tpe1 + "\n" +
    "and  " + sym2 + ": " + tpe2 + "\nmatch";

  /** Give a string representation of symbol `sym' with type `tp'
  *  for error diagnostics. `sym' may be null.
  */
  def toString(sym: Symbol, tp: Type): String =
    (if (tp.isInstanceOf[Type$OverloadedType]) "overloaded " else "") +
    (if (sym == null) "expression" else sym) + " of type " + tp;

// Variance calculation ----------------------------------------------------------

  private def flip(v: int): int = {
    if (v == COVARIANT) CONTRAVARIANT;
    else if (v == CONTRAVARIANT) COVARIANT;
    else v
  }

  private def cut(v: int): int = {
    if (v == VARIANCES) v
    else 0
  }

  /** Compute variances of all type parameters `tparams' in type `tp'.
  *  A variance is taken from the four point lattice
  *
  *  0, Modifiers.COVARIANT, Modifiers.CONTRAVARIANT, Modifiers.VARIANCES.
  */
  private def variance(tparams: Array[Symbol], tp: Type): Array[int] = {
    val vs: Array[int] = new Array[int](tparams.length);
    { var i = 0; while (i < vs.length) {
      vs(i) = variance(tparams(i), tp);
      i = i + 1;
    }}
    vs
  }

  /** Compute variances of all type parameters `tparams' in types `tps'.
  */
  private def variance(tparams: Array[Symbol], tps: Array[Type]): Array[int] = {
    val vs: Array[int] = new Array[int](tparams.length);
    { var i = 0; while (i < vs.length) {
      vs(i) = variance(tparams(i), tps);
      i = i + 1;
    }}
    vs
  }

  /** Compute variance of type parameter `tparam' in types of all symbols `sym'.
  */
  private def variance(tparam: Symbol, syms: Array[Symbol]): int = {
    var v: int = VARIANCES;
    { var i = 0; while (i < syms.length) {
      v = v & variance(tparam, syms(i));
      i = i + 1;
    }}
    v
  }

  /** Compute variance of type parameter `tparam' in type of symbol `sym'.
  */
  private def variance(tparam: Symbol, sym: Symbol): int = sym.kind match {
    case ERROR =>
      VARIANCES
    case VAL =>
      variance(tparam, sym.info())
    case TYPE =>
      variance(tparam, sym.info()) & flip(variance(tparam, sym.loBound()))
    case ALIAS =>
      cut(variance(tparam, sym.info()))
    case _ =>
      0
  }

  /** Compute variance of type parameter `tparam' in all types `tps'.
     */
  private def variance(tparam: Symbol, tps: Array[Type]): int = {
    var v: int = VARIANCES;
    { var i = 0; while (i < tps.length) {
      v = v & variance(tparam, tps(i));
      i = i + 1;
    }}
    v
  }

  /** Compute variance of type parameter `tparam' in all type arguments
  *  `tps' which correspond to formal type parameters `tparams'.
  */
  private def varianceInArgs(tvar: Symbol, tps: Array[Type], tparams: Array[Symbol]): int = {
    var v: int = VARIANCES;
    { var i = 0; while (i < tps.length) {
      if ((tparams(i).flags & COVARIANT) != 0) {
	v = v & variance(tvar, tps(i));
      } else if ((tparams(i).flags & CONTRAVARIANT) != 0) {
	v = v & flip(variance(tvar, tps(i)));
      } else {
	v = v & cut(variance(tvar, tps(i)));
      }
      i = i + 1;
    }}
    v
  }

  /** Does given `tparam' occur with variance `v' in type?
  */
  private def variance(tparam: Symbol, tp: Type): int = tp match {
    case Type.ErrorType | Type.AnyType | Type.NoType | Type.NoPrefix |
         Type$ThisType(_) | Type$ConstantType(_, _) =>
      VARIANCES
    case Type$TypeRef(pre, sym, args) =>
      if (sym == tparam) COVARIANT
      else variance(tparam, pre) & varianceInArgs(tparam, args, sym.typeParams())
    case Type$SingleType(pre, sym) =>
      cut(variance(tparam, pre))
    case Type$CompoundType(parts, members) =>
      variance(tparam, parts) & variance(tparam, members.elements())
    case Type$MethodType(params, restype) =>
      flip(variance(tparam, params)) & variance(tparam, restype)
    case Type$PolyType(tparams, restype) =>
      flip(variance(tparam, tparams)) & variance(tparam, restype)
  }

// Accessibility ------------------------------------------------------------

  /** Check that `sym' is accessible as a member of tree `site' in current context.
  */
  def checkAccessible(pos: int, sym: Symbol, symtype: Type, site: Tree, sitetype: Type): Type = {
    //System.out.println("check acc " + sym);//DEBUG
    if ((sym.owner().flags & INCONSTRUCTOR) != 0 &&
	!(sym.kind == TYPE && sym.isParameter()) &&
	site.isInstanceOf[Tree$This]) {
      error(pos, "" + sym + " cannot be accessed from constructor");
      Type.ErrorType;
    } else {
      symtype match {
	case Type$OverloadedType(alts, alttypes) =>
	  var nacc: int = 0;
	  var i = 0; while (i < alts.length) {
	    if (isAccessible(alts(i), site, sitetype))
	      nacc = nacc + 1;
	    i = i + 1
	  }
	  if (nacc == 0) {
	    error(pos, "" + sym + " cannot be accessed in " + sitetype.widen());
	    Type.ErrorType
	  } else {
	    val alts1: Array[Symbol] = new Array[Symbol](nacc);
	    val alttypes1: Array[Type] = new Array[Type](nacc);
	    nacc = 0;
	    var i = 0; while (i < alts.length) {
	      if (isAccessible(alts(i), site, sitetype)) {
		alts1(nacc) = alts(i);
		alttypes1(nacc) = alttypes(i);
		nacc = nacc + 1;
	      }
	      i = i + 1
	    }
	    new Type$OverloadedType(alts1, alttypes1)
	  }
	case _ =>
	  if (isAccessible(sym, site, sitetype)) {
	    symtype
	  } else {
	    error(pos, "" + sym + " cannot be accessed in " + sitetype.widen());
	    Type.ErrorType
	  }
      }
    }
  }

  /** Is `sym' accessible as a member of tree `site' in current context?
  */
  private def isAccessible(sym: Symbol, site: Tree, sitetype: Type): boolean = {

    /** Are we inside definition of `owner'?
    */
    def accessWithin(owner: Symbol): boolean = {
      var c: Context = getContext;
      while (c != Context.NONE && c.owner != owner) {
	c = c.outer.enclClass;
      }
      c != Context.NONE;
    }

    /** Is `clazz' a subclass of an enclosing class?
    */
    def isSubClassOfEnclosing(clazz: Symbol): boolean = {
      var c: Context = getContext;
      while (c != Context.NONE && !clazz.isSubClass(c.owner)) {
	c = c.outer.enclClass;
      }
      c != Context.NONE;
    }

    (sym.flags & (PRIVATE | PROTECTED)) == 0
    ||
    {val owner = if (sym.isConstructor()) sym.constructorClass()
		  else sym.owner();
     accessWithin(owner)
     ||
     ((sym.flags & PRIVATE) == 0) &&
     (site.isInstanceOf[Tree$Super] ||
      (sitetype.symbol().isSubClass(owner) &&
       isSubClassOfEnclosing(sitetype.symbol())))
    }
  }

// Views ---------------------------------------------------------------------

  val memberViewCache = new HashMap[Symbol, List[View]];

//  private def memberViews(tp: Type): List[View] = List();

  private def memberViews(tp: Type): List[View] = {
    val tpsym = tp.widen().symbol();
    memberViewCache.get(tpsym) match {
      case Some(vs) => vs
      case None =>
	var vs = companionObjViews(tp, tpsym);
	val ps = tpsym.parents();
	var i = ps.length - 1; while (i >= 0) {
	  vs = memberViews(ps(i)) ::: vs;
	  i = i - 1
	}
      memberViewCache.update(tpsym, vs);
      vs
    }
  }

  private def companionObjViews(tp: Type, clazz: Symbol): List[View] = {
    if (clazz.kind == CLASS && !clazz.isModuleClass() && !clazz.isCaseClass()) {
      var obj = clazz.owner().info().lookupNonPrivate(clazz.name.toTermName());
      if (obj.isExternal() == clazz.isExternal()) {
	//System.out.println("comp obj view " + tp + " " + obj);//DEBUG
	obj.getType() match {
	  case Type$OverloadedType(alts, alttypes) =>
	    var i = 0; while (i < alts.length) {
	      if (alts(i).isModule()) obj = alts(i);
	      i = i + 1
	    }
	  case _ =>
	}
	if (obj.isModule()) {
	  val qual = if (tp.prefix() == Type.NoType) Tree.Empty
		     else gen.mkRef(Position.NOPOS, tp.prefix(), obj);
	  val viewsym = obj.info().lookupNonPrivate(Names.view);
	  if (viewsym.kind == VAL) {
	    obj.getType().memberType(viewsym) match {
	      case Type$OverloadedType(alts, alttypes) =>
		var i = alttypes.length - 1;
		var vs: List[View] = List();
		while (i >= 0) {
		  vs = View(alts(i), alttypes(i), qual, Context.NONE) :: vs;
		  i = i - 1
		}
		vs
	      case viewtype =>
		List(View(viewsym, viewtype, qual, Context.NONE))
	    }
	  } else List()
	} else List()
      } else List()
    } else List()
  }

  private def getViews(tp: Type): List[View] = {
    memberViews(tp) ::: getContext.viewMeths;
  }

  def viewExpr(pos: int, v: View): Tree = {
    val qual = v.qual.duplicate();
    qual.pos = pos;
    val viewType = checkAccessible(
      pos, v.sym, v.symtype, qual, qual.getType());
    make.Select(pos, qual, v.sym.name)
      .setSymbol(v.sym).setType(viewType)
  }

  def viewArg(pos: int, vtype: Type, targs: Array[Type]): Tree = {
    val vargs = vtype.typeArgs();
    val v = bestView(vargs(0), vargs(1), Names.EMPTY);
    if (v != null) {
      var vtree = viewExpr(pos, v);
      vtree.getType() match {
	case Type$PolyType(vtparams, vrestype) =>
	  assert(vtparams.length != 0);
	  vtree = exprInstance(vtree, vtparams, vrestype, vtype);
	  assert(vtree.getType().isSubType(vtype));
      }
      vtree
    } else {
      error(pos,
	    "type instantiation with [" +
	    ArrayApply.toString(targs.asInstanceOf[Array[Object]], ",") +
	    "] failed since " + vargs(0) + " is not viewable as " + vargs(1));
    }
  }

  def addViewArgs(tree: Tree, tparams: Array[Symbol], restype: Type, targs: Array[Type]): Tree = {
    restype match {
      case Type$MethodType(params, _) =>
	val viewargs = new Array[Tree](params.length);
	var i = 0; while (i < params.length) {
	  viewargs(i) = viewArg(
	    tree.pos, params(i).getType().subst(tparams, targs), targs);
	  i = i + 1
	}
	gen.Apply(tree, viewargs);
      case Type.ErrorType =>
	tree
    }
  }

// Type parameter inference -----------------------------------------------------

  private class NoInstance(msg: String) extends RuntimeException(msg);

  /** map every TypeVar to its constraint.inst field.
  *  throw a NoInstance exception if a NoType or AnyType is encountered.
  */
  private val instantiateMap: Type$Map = new Type$Map() {
    def apply(t: Type): Type = instantiate(t)
  }

  private def instantiate(tp: Type): Type = tp match {
    case Type.AnyType | Type.NoType =>
      throw new NoInstance("undetermined type");
    case Type$TypeVar(origin, constr) =>
      if (constr.inst != Type.NoType) instantiate(constr.inst)
      else throw new NoInstance("no unique instantiation of type variable " +
				origin + " could be found");
    case _ =>
      instantiateMap.map(tp)
  }

  /** Map type variable to its instance, or, if `covariant' is true,
  *  to its upper bound;
  */
  private def instantiateToBound(tp: Type, variance: int): Type = tp match {
    case Type$TypeVar(origin, constr) =>
      try {
	if (constr.inst != Type.NoType) {
	  instantiate(constr.inst)
	} else if ((variance & COVARIANT) != 0 && constr.hibounds != Type$List.EMPTY) {
	  maximizeVar(tp);
	  instantiate(constr.inst)
	} else if ((variance & CONTRAVARIANT) != 0 && constr.lobounds != Type$List.EMPTY) {
	  minimizeVar(tp);
	  instantiate(constr.inst)
	} else {
	  Type.AnyType;
	}
      } catch {
	case ex: NoInstance => Type.AnyType
      }
  }

  /** The formal parameter types corresponding to `params'.
  *  If `params' has a repeated last parameter, a list of
  *  (nargs - params.length + 1) copies of its type is returned.
  */
  def formalTypes(params: Array[Symbol], nargs: int): Array[Type] = {
    if (params.length > 0 && (params(params.length-1).flags & REPEATED) != 0) {
      val args: Array[Type] = params(params.length-1).getType().typeArgs();
      if (args.length == 1) {
	val ft: Type = args(0); // last param has type Seq[T], we need T here
	val formals: Array[Type] = new Array[Type](nargs);
	var i = 0;
	while (i < params.length-1) {
	  formals(i) = params(i).getType();
	  i = i + 1;
	}
	while (i < nargs) {
	  formals(i) = ft;
	  i = i + 1;
        }
	return formals;
      }
    }
    Symbol.getType(params);
  }

  /** Is type fully defined, i.e. no embedded anytypes or typevars in it?
  */
  def isFullyDefined(tp: Type): boolean = {
    try {
      instantiate(tp);
      true
    } catch {
      case ex: NoInstance => false
    }
  }

  /** Do type arguments `targs' conform to formal parameters `tparams'?
  */
  private def isWithinBounds(tparams: Array[Symbol], targs: Array[Type]): boolean = {
    var i = 0;
    while (i < targs.length) {
      val hibound: Type = tparams(i).info().subst(tparams, targs);
      if (!targs(i).isSubType(hibound)) {
	var j = 0;
	while (j < tparams.length) {
	  if (hibound.symbol() == tparams(j))
	    return isWithinBounds(
	      tparams,
	      Type.subst(
		targs,
		NewArray.Symbol(tparams(j)),
		NewArray.Type(targs(i))));
	  j = j + 1
	}
	return false;
      }
      val lobound: Type = tparams(i).loBound().subst(tparams, targs);
      if (!lobound.isSubType(targs(i))) {
	var j = 0;
	while (j < tparams.length) {
	  if (lobound.symbol() == tparams(j))
	    return isWithinBounds(
	      tparams,
	      Type.subst(
		targs,
		NewArray.Symbol(tparams(j)),
		NewArray.Type(targs(i))));
	  j = j + 1
	}
	return false
      }
      i = i + 1
    }
    true
  }

  /** throw a type error if arguments not within bounds.
  */
  def checkBounds(tparams: Array[Symbol], targs: Array[Type], prefix: String): unit =
    if (!isWithinBounds(tparams, targs))
      throw new Type$Error(
	prefix + "type arguments " +
	ArrayApply.toString(targs.asInstanceOf[Array[Object]], "[", ",", "]") +
	" do not conform to " +
	tparams(0).owner() + "'s type parameter bounds " +
	ArrayApply.toString(Symbol.defString(tparams).asInstanceOf[Array[Object]], "[", ",", "]"));

  /** Instantiate variable to glb of its high bounds.
  */
  private def maximizeVar(tp: Type): unit = tp match {
    case Type$TypeVar(origin, constr) =>
      if (constr.inst == Type.NoType)
	constr.inst = Type.glb(constr.hibounds.toArray());
  }

  /** Instantiate variable to lub of its low bounds.
  */
  private def minimizeVar(tp: Type): unit = tp match {
    case Type$TypeVar(origin, constr) =>
      if (constr.inst == Type.NoType)
	constr.inst = Type.lub(constr.lobounds.toArray());
  }

  /** Solve constraint collected in types `tvars', instantiating `tvars(i)'
  *  in the process.
  *  @param tparams    The type parameters corresponding to `tvars'
  *  @param upper      When `true' search for max solution else min.
  *  @param variances  The variances of type parameters; need to reverse
  *                    solution direction for all contravariant variables.
  *  @param tvars      All type variables to be instantiated.
  *  @param i          The index of the type variable to be instantiated.
  */
  private def solve(tparams: Array[Symbol], upper: boolean, variances: Array[int], tvars: Array[Type], i: int): unit = {
    if (tvars(i) != Type.NoType) {
      tvars(i) match {
	case Type$TypeVar(origin, constr) =>
	  if (constr.inst != Type.NoType) {
	    constr.inst = instantiate(constr.inst);
	    tvars(i) = constr.inst;
	  } else {
	    val tvar: Type = tvars(i);
	    val up: boolean = if (variances(i) != CONTRAVARIANT) upper
			      else !upper;
	    tvars(i) = Type.NoType;
	    val bound: Type = if (up) tparams(i).info() else tparams(i).loBound();
	    var cyclic: boolean = false;
	    { var j = 0; while (j < tvars.length) {
	      if (bound.contains(tparams(j)) ||
		  up && tparams(j).loBound().isSameAs(tparams(i).getType()) ||
		  !up && tparams(j).info().isSameAs(tparams(i).getType())) {
		    cyclic = cyclic | tvars(j) == Type.NoType;
		    solve(tparams, upper, variances, tvars, j);
	      }
	      j = j + 1
	    }}
	    if (!cyclic) {
	      if (up) {
		if (bound.symbol() != definitions.ANY_CLASS)
		  constr.hibounds = new Type$List(
		    bound.subst(tparams, tvars), constr.hibounds);
		{ var j = 0; while (j < tvars.length) {
		  if (tparams(j).loBound().isSameAs(
		    tparams(i).getType())) {
		      constr.hibounds = new Type$List(
			tparams(j).getType().subst(tparams, tvars),
			constr.hibounds);
		  }
		  j = j + 1
		}}
	      } else {
		if (bound.symbol() != definitions.ALL_CLASS)
		  constr.lobounds = new Type$List(
		    bound.subst(tparams, tvars), constr.lobounds);
		{ var j = 0; while (j < tvars.length) {
		  if (tparams(j).info().isSameAs(
		    tparams(i).getType())) {
		      constr.lobounds = new Type$List(
			tparams(j).getType().subst(tparams, tvars),
			constr.lobounds);
		  }
		  j = j + 1
		}}
	      }
	    }
	    if (up) maximizeVar(tvar);
	    else minimizeVar(tvar);
	    tvars(i) = tvar.asInstanceOf[Type$TypeVar].constr.inst;
	  }
	case _ =>
      }
    }
  }

  /** Generate an array of fresh type variables corresponding to parameters
  *  `tparams'
  */
  private def freshVars(tparams: Array[Symbol]): Array[Type] = {
    val tvars: Array[Type] = new Array[Type](tparams.length);
    { var i = 0; while (i < tvars.length) {
      tvars(i) = new Type$TypeVar(tparams(i).getType(), new Type$Constraint());
      i = i + 1
    }}
    tvars
  }

  private val freshInstanceMap: Type$Map = new Type$Map() {
    def apply(t: Type): Type = t match {
      case Type$PolyType(tparams, restp) =>
	val restp1: Type = apply(restp);
	var tparams1: Array[Symbol] = Symbol.EMPTY_ARRAY;
	var newparams1: Array[Symbol] = Symbol.EMPTY_ARRAY;
	restp1 match {
	  case Type$PolyType(_, _) =>
	    // If there is a nested polytype, we need to
	    // substitute also its new type parameters for its old ones
	    // here. Reason: The outer polytype may refer to type
	    // variables of the inner one.
	    tparams1 = restp.typeParams();
	    newparams1 = restp1.typeParams();
	  case _ =>
	}
	val newparams: Array[Symbol] = new Array[Symbol](tparams.length);
	{ var i = 0; while (i < tparams.length) {
	  newparams(i) = tparams(i).cloneSymbol();
	  i = i + 1
	}}
	{ var i = 0; while (i < tparams.length) {
	  newparams(i).setInfo(
	    newparams(i).info()
	    .subst(tparams, newparams)
	    .subst(tparams1, newparams1));
	  newparams(i).setLoBound(
	    newparams(i).loBound()
	    .subst(tparams, newparams)
	    .subst(tparams1, newparams1));
	  i = i + 1
	}}
	new Type$PolyType(newparams, restp1.subst(tparams, newparams))

      case Type$OverloadedType(_, _) =>
	map(t)

      case _ =>
	t
    }
  }

  def freshInstance(tp: Type): Type = freshInstanceMap.apply(tp);

  /** Automatically perform the following conversions on expression types:
  *  A method type becomes the corresponding function type.
  *  A nullary metAhod type becomes its result type.
  */
  private def normalize(tp: Type): Type = tp match {
    case Type$MethodType(params, restype) =>
      definitions.FUNCTION_TYPE(
	Symbol.getType(params), normalize(restype));
    case Type$PolyType(tparams, restype) if (tparams.length == 0) =>
      normalize(restype);
    case _ =>
      tp
  }

  /** Is normalized type `tp' a subtype of prototype `pt'?
  */
  def isCompatible(tp: Type, pt: Type): boolean =
    isCompatible(tp, pt, true);

  def isCompatible(tp: Type, pt: Type, regularValue: boolean): boolean = {
    def canView(tp: Type): boolean = tp match {
      case Type$OverloadedType(_, alttypes) =>
	var i = 0;
	while (i < alttypes.length && !canView(alttypes(i))) i = i + 1;
	i < alttypes.length
      case Type$PolyType(tparams, restype) if tparams.length == 0 =>
	restype.isSubType(pt)
      case _ =>
	false
    }
    val tp1 = normalize(tp);
    if (tp1.isSubType(pt)) true
    else if (regularValue) {
      val argtypes = NewArray.Type(tp1);
      var viewMeths = getViews(tp1);
      while (!viewMeths.isEmpty &&
	     !isApplicable(viewMeths.head.symtype, argtypes, pt, Names.EMPTY, false))
	viewMeths = viewMeths.tail;
      if (!viewMeths.isEmpty) true
      // todo: remove
      else {
	val coerceMeth: Symbol = tp1.lookup(Names.coerce);
        coerceMeth.kind != NONE && canView(tp1.memberType(coerceMeth));
      }
    } else false;
  }

  def isCompatible(tps: Array[Type], pts: Array[Type]): boolean =
    isCompatible(tps, pts, true);

  def isCompatible(tps: Array[Type], pts: Array[Type], regularValue: boolean): boolean = {
    var i = 0; while (i < tps.length) {
      if (!isCompatible(tps(i), pts(i), regularValue)) return false;
      i = i + 1
    }
    true
  }

  /** Type arguments mapped to `scala.All' are taken to be uninstantiated.
  *  Map all those type arguments to their corresponding type parameters
  *  and return all these type parameters as result.
  */
  private def normalizeArgs(targs: Array[Type], tparams: Array[Symbol]): Array[Symbol] = {
    var uninstantiated: Type$List = Type$List.EMPTY;
    { var i = 0; while (i < targs.length) {
      if (targs(i).symbol() == definitions.ALL_CLASS) {
	targs(i) = tparams(i).getType();
	uninstantiated = Type$List.append(uninstantiated, targs(i));
      }
      i = i + 1
    }}
    Type.symbol(uninstantiated.toArray());
  }

  /** Return inferred type arguments of polymorphic expression, given
  *  its type parameters and result type and a prototype `pt'.
  *  If no minimal type variables exist that make the
  *  instantiated type a subtype of `pt', return `null'.
  */
  private def exprTypeArgs(tparams: Array[Symbol], restype: Type, pt: Type): Array[Type] =
    exprTypeArgs(tparams, restype, pt, true);

  private def exprTypeArgs(tparams: Array[Symbol], restype: Type,
			   pt: Type, regularValue: boolean): Array[Type] = {
    val tvars: Array[Type] = freshVars(tparams);
    val insttype: Type = restype.subst(tparams, tvars);
    if (isCompatible(insttype, pt, regularValue)) {
      try {
	val restype1 = normalize(restype);
	{ var i = 0; while (i < tvars.length) {
	  solve(tparams, false, variance(tparams, restype1), tvars, i);
	  i = i + 1
	}}
	tvars
      } catch {
	case ex: NoInstance => null
      }
    } else {
	null
    }
  }

  /** Return inferred proto-type arguments of function, given
  *  its type and value parameters and result type, and a
  *  prototype `pt' for the function result.
  *  Type arguments need to be either determined precisely by
  *  the prototype, or they are maximized, if they occur only covariantly
  *  in the value parameter list.
  *  If instantiation of a type parameter fails,
  *  take Type.AnyType for the proto-type argument.
  */
  def protoTypeArgs(tparams: Array[Symbol], restype: Type, pt: Type, params: Array[Symbol]): Array[Type] = {
    val tvars: Array[Type] = freshVars(tparams);
    val insttype: Type = restype.subst(tparams, tvars);
    val targs: Array[Type] = new Array[Type](tvars.length);
    { var i = 0; while (i < tvars.length) {
      targs(i) = Type.AnyType;
      i = i + 1
    }}
    if (isCompatible(insttype, pt)) {
      try {
	{ var i = 0; while (i < tvars.length) {
	  targs(i) = instantiateToBound(
	    tvars(i), variance(tparams(i), params));
	  i = i + 1
	}}
      } catch {
	case ex: NoInstance =>
	  { var i = 0; while (i < tvars.length) {
	    targs(i) = Type.AnyType;
	    i = i + 1
	  }}
      }
    }
    targs
  }

  /** Return inferred type arguments, given type parameters, formal parameters,
  *  argument types, result type and expected result type.
  *  If this is not possible, throw a `NoInstance' exception, or, if
  *  `needToSucceed' is false alternatively return `null'.
  *  Undetermined type arguments are represented by `definitions.ALL_TYPE'.
  *  No check that inferred parameters conform to their bounds is made here.
  */
  private def methTypeArgs(tparams: Array[Symbol], params: Array[Symbol],
			   argtypes: Array[Type], restp: Type,
			   pt: Type,
			   needToSucceed: boolean, regularValue: boolean): Array[Type] = {
    //System.out.println("methTypeArgs, tparams = " + ArrayApply.toString(tparams) + ", params = " + ArrayApply.toString(params) + ", type(params) = " + ArrayApply.toString(Symbol.type(params)) + ", argtypes = " + ArrayApply.toString(argtypes));//DEBUG

    val tvars: Array[Type] = freshVars(tparams);
    val formals: Array[Type] = formalTypes(params, argtypes.length);
    if (formals.length != argtypes.length) {
      if (needToSucceed)
	throw new NoInstance("parameter lists differ in length");
      return null;
    }

    // check first whether type variables can be fully defined from
    // expected result type.
    if (!isCompatible(restp.subst(tparams, tvars), pt, regularValue)) {
      if (needToSucceed)
	throw new NoInstance("result type " + restp +
			     " is incompatible with expected type " + pt);
      return null;
    }
    { var i = 0; while (i < tvars.length) {
      val tvar: Type$TypeVar = tvars(i).asInstanceOf[Type$TypeVar];
      if (!isFullyDefined(tvar)) tvar.constr.inst = Type.NoType;
      i = i + 1
    }}

    // Then define remaining type variables from argument types.
    var i = 0;
    while (i < argtypes.length) {
      if (!isCompatible(argtypes(i).widen().subst(tparams, tvars),
			formals(i).subst(tparams, tvars),
		        regularValue)) {
	if (needToSucceed) {
	  if (global.explaintypes) {
	    Type.explainSwitch = true;
	    argtypes(i).widen().subst(tparams, tvars).isSubType(
	      formals(i).subst(tparams, tvars));
	    Type.explainSwitch = false;
	  }
	  throw new NoInstance(
	    typeErrorMsg(
	      "argument expression's type is not compatible with formal parameter type",
	      argtypes(i).widen().subst(tparams, tvars),
	      formals(i).subst(tparams, tvars)));
	}
	return null;
      }
      i = i + 1;
    }
    { var i = 0; while (i < tvars.length) {
      solve(tparams, false, variance(tparams, formals), tvars, i);
      i = i + 1
    }}
    //System.out.println(" = " + ArrayApply.toString(tvars));//DEBUG
    tvars
  }

  /** Create and attribute type application node. Pass arguments for that
  *  `tparams' prefix which is owned by the tree's symbol. If there are remaining
  *  type parameters, substitute corresponding type arguments for them in the
  *  tree. Such remaining type parameters always come from an inferred PolyType.
  */
  def mkTypeApply(tree: Tree, tparams: Array[Symbol], restype: Type, targs: Array[Type]): Tree = {
    var tree1: Tree = tree;
    val sym: Symbol = tree.symbol();
    var i: int = 0;
    while (i < tparams.length && tparams(i).owner() == sym)
      i = i + 1;
    if (i < tparams.length) {
      //System.out.println("tpar " + tparams(i) + " of " + tparams(i).owner() + " <> " + sym);//DEBUG
      //new Printer().print(tree1);//DEBUG
      //System.out.println(ArrayApply.toString(targs) + "/" + i + "/" + ArrayApply.toString(tparams));//DEBUG
      val tparams1: Array[Symbol] = new Array[Symbol](tparams.length - i);
      System.arraycopy(tparams, i, tparams1, 0, tparams1.length);
      val targs1: Array[Type] = new Array[Type](tparams.length - i);
      System.arraycopy(targs, i, targs1, 0, targs1.length);
      tree1 = substituter.apply(tree1, tparams1, targs1);
    }
    if (0 < i) {
      val argtrees: Array[Tree] = new Array[Tree](i);
      var viewbounded = false;
      { var j = 0; while (j < i) {
	argtrees(j) = gen.mkType(tree.pos, targs(j));
	viewbounded = viewbounded | ((tparams(j).flags & VIEWBOUND) != 0);
	j = j + 1
      }}
      tree1 = make.TypeApply(tree.pos, tree1, argtrees)
	.setType(restype.subst(tparams, targs));
      if (viewbounded)
	tree1 = addViewArgs(tree1, tparams, restype, targs);
    }
    tree1
  }

  /** Return the instantiated and normalized type of polymorphic expression
  *  with type `[tparams]restype', given two prototypes `pt1', and `pt2'.
  *  `pt1' is the strict first attempt prototype where type parameters
  *  are left unchanged. `pt2' is the fall-back prototype where type parameters
  *  are replaced by `AnyType's. We try to instantiate first to `pt1' and then,
  *  if this fails, to `pt2'. If both attempts fail, a Type.Error is thrown.
  */
  def argumentTypeInstance(tparams: Array[Symbol], restype: Type, pt1: Type, pt2: Type): Type = {
    restype match {
      case Type$PolyType(tparams1, restype1) =>
	val tparams2: Array[Symbol] = new Array[Symbol](tparams.length + tparams1.length);
	System.arraycopy(tparams, 0, tparams2, 0, tparams.length);
	System.arraycopy(tparams1, 0, tparams2, tparams.length, tparams1.length);
	argumentTypeInstance(tparams2, restype1, pt1, pt2);

      case _ =>
	if (tparams.length != 0) {
	  var targs: Array[Type] = exprTypeArgs(tparams, restype, pt1);
	  if (targs == null)
	    targs = exprTypeArgs(tparams, restype, pt2);
	  if (targs == null)
	    throw new Type$Error(
	      typeErrorMsg(
		"polymorphic argument cannot be instantiated to formal parameter type",
		new Type$PolyType(tparams, restype), pt2));
	  checkBounds(tparams, targs, "inferred ");
	  restype.subst(tparams, targs);
	} else {
	  normalize(restype);
	}
    }
  }

  /** Instantiate expression `tree' of polymorphic type [tparams]restype,
  *  using prototype `pt'.
  */
  def exprInstance(tree: Tree, tparams: Array[Symbol], restype: Type, pt: Type): Tree = {
    restype match {
      case Type$PolyType(tparams1, restype1) =>
	val tparams2: Array[Symbol] = new Array[Symbol](tparams.length + tparams1.length);
	System.arraycopy(tparams, 0, tparams2, 0, tparams.length);
	System.arraycopy(tparams1, 0, tparams2, tparams.length, tparams1.length);
	exprInstance(tree, tparams2, restype1, pt)
      case _ =>
	val targs: Array[Type] = exprTypeArgs(tparams, restype, pt);
	if (targs == null)
	  throw new Type$Error(
	    "polymorphic expression of type " + tree.getType() +
	    " cannot be instantiated from expected type " + pt);
	checkBounds(tparams, targs, "inferred ");
	mkTypeApply(tree, tparams, restype, targs)
    }
  }

  /** Instantiate method `tree' of polymorphic type [tparams]restype,
  *  so that resulting method type can be applied to arguments with
  *  types `argtypes' and its result type is compatible with `pt'.
  */
  def methodInstance(tree: Tree, tparams: Array[Symbol], restype: Type, argtypes: Array[Type], pt: Type): Tree = {
    restype match {
      case Type$PolyType(tparams1, restype1) =>
	val tparams2: Array[Symbol] = new Array[Symbol](tparams.length + tparams1.length);
	System.arraycopy(tparams, 0, tparams2, 0, tparams.length);
	System.arraycopy(tparams1, 0, tparams2, tparams.length, tparams1.length);
	methodInstance(tree, tparams2, restype1, argtypes, pt)

      case Type$MethodType(params, restpe) =>
	var targs: Array[Type] = _;
	try {
	  targs = methTypeArgs(tparams, params, argtypes, restpe, pt, true, false);
	} catch {
	  case ex: NoInstance =>
	    throw new Type$Error(
	      applyErrorMsg(
		"no type parameters for ", tree,
		" exist so that it can be applied to arguments ",
		Type.widen(argtypes), Type.AnyType) +
	      "\n --- because ---\n" + ex.getMessage());
	}
	val uninstantiated: Array[Symbol] = normalizeArgs(targs, tparams);
	checkBounds(tparams, targs, "inferred ");
	val restype1: Type =
	  if (uninstantiated.length == 0) restype
	  else new Type$MethodType(
	    params, new Type$PolyType(uninstantiated, restpe));
	mkTypeApply(tree, tparams, restype1, targs);
      case _ =>
	tree
    }
  }

  /** Instantiate constructor `tree' of polymorphic type [tparams]restype',
  *  so that its the result type of `restype' matches prototype `pt'.
  *  If constructor is polymorphic, maximize all type variables under this
  *  condition.
  */
  def constructorInstance(tree: Tree, tparams: Array[Symbol], restype: Type, pt: Type): unit = {
    restype match {
      case Type$PolyType(tparams1, restype1) =>
	val tparams2: Array[Symbol] = new Array[Symbol](tparams.length + tparams1.length);
	System.arraycopy(tparams, 0, tparams2, 0, tparams.length);
	System.arraycopy(tparams1, 0, tparams2, tparams.length, tparams1.length);
	constructorInstance(tree, tparams2, restype1, pt)

      case _ =>
	val tvars: Array[Type] = freshVars(tparams);
	val restype1: Type = restype.subst(tparams, tvars);
	val ctpe1: Type = restype1.resultType();
	if (ctpe1.isSubType(pt)) {
	  try {
	    { var i = 0; while (i < tvars.length) {
	      solve(tparams, true, variance(tparams, restype.resultType()),
		    tvars, i);
	      i = i + 1
	    }}
	    checkBounds(tparams, tvars, "inferred ");
	    tree.setType(restype.subst(tparams, tvars));
	    //System.out.println("inferred constructor type: " + tree.getType());//DEBUG
	  } catch {
	    case ex: NoInstance =>
	      throw new Type$Error(
		"constructor of type " + ctpe1 +
		" can be instantiated in more than one way to expected type " +
		pt +
		"\n --- because ---\n" + ex.getMessage());
	  }
	} else {
	  throw new Type$Error(
	    typeErrorMsg(
	      "constructor cannot be instantiated to expected type",
	      ctpe1, pt));
	}
    }
  }

// Overload Resolution -------------------------------------------------------------

  /** Is function type `ftpe' applicable to `argtypes' and
  *  does its result conform to `pt'?
  */
  def isApplicable(ftpe: Type, argtypes: Array[Type], pt: Type): boolean =
    isApplicable(ftpe, argtypes, pt, Names.EMPTY, true);

  def isApplicable(ftpe: Type, argtypes: Array[Type], pt: Type,
		   fieldName: Name, regularValue: boolean): boolean = ftpe match {
    case Type$MethodType(params, restpe) =>
      // sequences ? List( a* )
      val formals: Array[Type] = formalTypes(params, argtypes.length);
      formals.length == argtypes.length &&
      isCompatible(argtypes, formals, regularValue) &&
      isCompatible(restpe, pt, regularValue) &&
      (fieldName == Names.EMPTY || restpe.lookup(fieldName).kind != NONE)
    case Type$PolyType(tparams, Type$MethodType(params, restpe)) =>
      try {
	val targs: Array[Type] = methTypeArgs(
	  tparams, params, argtypes, restpe, pt, false, regularValue);
	if (targs != null) {
	  val uninstantiated: Array[Symbol] = normalizeArgs(targs, tparams);
	  isWithinBounds(tparams, targs) &&
	  exprTypeArgs(uninstantiated,
		       restpe.subst(tparams, targs), pt,
		       regularValue) != null &&
	  (fieldName == Names.EMPTY ||
	   restpe.subst(tparams, targs).lookup(fieldName).kind != NONE)
	} else {
	  false
	}
      } catch {
	case ex: NoInstance => false
      }
    case _ =>
      if (!regularValue) {
	val ftpe1 = applyType(ftpe);
	ftpe1 != ftpe &&
	isApplicable(ftpe1, argtypes, pt, fieldName, false);
      } else false
  }

  def applyType(tp: Type) =
    if (tp.isObjectType()) {
      val applyMeth: Symbol = tp.lookup(Names.apply);
      if (applyMeth != Symbol.NONE) tp.memberType(applyMeth)
      else tp
    } else tp;

  /** Does type `ftpe1' specialize type `ftpe2'
  *  when both are alternatives in an overloaded function?
  */
  def specializes(ftpe1: Type, ftpe2: Type): boolean = ftpe1 match {
    case Type$MethodType(params, _) =>
      isApplicable(ftpe2, Symbol.getType(params), Type.AnyType)
    case Type$PolyType(_, Type$MethodType(params, _)) =>
      isApplicable(ftpe2, Symbol.getType(params), Type.AnyType);
    case _ =>
      false
  }

  def specializesView(ftpe1: Type, ftpe2: Type): boolean =
    specializes(applyType(ftpe1), applyType(ftpe2));

  /** Assign `tree' the type of the alternative which matches
  *  prototype `pt', if it exists.
  *  If several alternatives match `pt', take unique parameterless one.
  *  Throw a Type.Error if several such alternatives exist.
  *  If no alternative matches, leave `tree' unchanged.
  */
  def exprAlternative(tree: Tree, alts: Array[Symbol], alttypes: Array[Type], pt: Type): unit = {

    def improves(tp1: Type, tp2: Type): boolean =
      !normalize(tp2).isSubType(pt) && normalize(tp1).isSubType(pt) ||
      tp2.isParameterized() &&
      (!tp1.isParameterized() || specializes(tp1, tp2));

    // first, catch the case of a missing parameter
    // list for an overloaded constructor.
    if (alts.length > 0) {
      var i: int = 0;
      while (i < alts.length && alts(i).isConstructor() && alttypes(i).isInstanceOf[Type$MethodType])
	i = i + 1;
      if (i == alts.length)
	throw new Type$Error("missing rguments for " + alts(0));
    }
    // second, do the normal case.
    var best: int = -1;
    { var i = 0; while (i < alttypes.length) {
      if (isCompatible(alttypes(i), pt) && (best < 0 || improves(alttypes(i), alttypes(best)))) {
	best = i;
      }
      i = i + 1
    }}
    { var i = 0; while (i < alttypes.length) {
      if (isCompatible(alttypes(i), pt) && (best < 0 || improves(alttypes(i), alttypes(best)))) {
	best = i;
      }
      i = i + 1
    }}
    if (best >= 0) {
      { var i = 0; while (i < alttypes.length) {
	if (isCompatible(alttypes(i), pt) && best != i && !improves(alttypes(best), alttypes(i))) {
	  throw new Type$Error(
	    overloadResolveErrorMsg(
	      alts(best), alttypes(best), alts(i), alttypes(i)) +
	    " expected type " + pt);
	}
	i = i + 1
      }}
      tree.setSymbol(alts(best)).setType(alttypes(best));
    }
  }

  /** Assign `tree' the type of an alternative
  *  which is applicable to `argtypes', and whose result type is
  *  a subtype of `pt' if it exists.
  *  If several applicable alternatives exist, take the
  *  most specialized one, or throw an error if no
  *  most specialized applicable alternative exists.
  *  If no alternative matches, leave `tree' unchanged,
  *  try to select method with pt = AnyType.
  *  If pt is AnyType, leave tree unchanged.
  */
  def methodAlternative(tree: Tree, alts: Array[Symbol], alttypes: Array[Type], argtypes: Array[Type], pt: Type): unit = {
    if (alts.length == 1) {
      tree.setSymbol(alts(0)).setType(alttypes(0));
      return;
    }
    var best: int = -1;
    { var i = 0; while (i < alttypes.length) {
      if (isApplicable(alttypes(i), argtypes, pt) &&
	  (best < 0 || specializes(alttypes(i), alttypes(best))))
	best = i;
      i = i + 1
    }}
    if (best >= 0) {
      { var i = 0; while (i < alttypes.length) {
	if (i != best &&
	    isApplicable(alttypes(i), argtypes, pt) &&
	    !(specializes(alttypes(best), alttypes(i)) &&
	      !specializes(alttypes(i), alttypes(best))))
	   throw new Type$Error(
	     overloadResolveErrorMsg(
	       alts(best), alttypes(best), alts(i), alttypes(i)) +
	     " argument types " +
	     ArrayApply.toString(argtypes.asInstanceOf[Array[Object]], "(", ",", ")") +
	     (if (pt == Type.AnyType) "" else " and expected result type " + pt));
	i = i + 1
      }}
      tree.setSymbol(alts(best)).setType(alttypes(best));
    } else if (pt != Type.AnyType) {
      methodAlternative(tree, alts, alttypes, argtypes, Type.AnyType);
    }
  }

  /** Assign `tree' the type of unique polymorphic alternative with `nparams'
  *  as the number of type parameters, if it exists.
  *  Throw error if several such polymorphic alternatives exist.
  *  If no alternative matches, leave `tree' unchanged.
  */
  def polyAlternative(tree: Tree, alts: Array[Symbol], alttypes: Array[Type], nparams: int): unit = {
    if (alts.length == 1) {
      tree.setSymbol(alts(0)).setType(alttypes(0));
      return;
    }
    var i: int = 0;
    while (i < alttypes.length &&
	   !(alts(i).isValue() &&
	     alttypes(i).typeParams().length == nparams))
      i = i + 1;

    if (i < alttypes.length) {
      { var j = i + 1; while (j < alttypes.length) {
	if (alts(j).isValue() &&
	    alttypes(j).typeParams().length == nparams)
	  throw new Type$Error(
	    overloadResolveErrorMsg(alts(i), alttypes(i), alts(j), alttypes(j)) +
	    " polymorphic function with " + nparams + " parameters");
	j = j + 1
      }}
      tree.setSymbol(alts(i)).setType(alttypes(i));
    }
  }

  /** return view which best matches argument type `tp' and has `name' as member.
  */
  def bestView(tp: Type, pt: Type, name: Name): View = {
    var best: View = null;
    var viewMeths = getViews(tp);
    val argtypes = NewArray.Type(tp);
    while (!viewMeths.isEmpty) {
      if (isApplicable(viewMeths.head.symtype, argtypes, pt, name, false) &&
	  (best == null || specializesView(viewMeths.head.symtype, best.symtype)))
	best = viewMeths.head;
      viewMeths = viewMeths.tail
    }
    if (best != null) {
      viewMeths = getViews(tp);
      while (!viewMeths.isEmpty) {
	if (viewMeths.head != best &&
	    isApplicable(viewMeths.head.symtype, argtypes, pt, name, false) &&
	    !(specializesView(best.symtype, viewMeths.head.symtype) &&
	      !specializesView(viewMeths.head.symtype, best.symtype))) {
	  throw new Type$Error(
	    "ambiguous view,\n" +
	    "both " + viewMeths.head.sym + ": " +
	    viewMeths.head.symtype + viewMeths.head.sym.locationString() + "\n" +
            "and  " + best.sym + ": " + best.symtype + best.sym.locationString() +
	    (if (name == Names.EMPTY)
	       "\nmap argument type " + tp.widen() + " to expected type " + pt
	     else
	       "\nadd member `" + name + "' to argument type " + tp.widen()))
	}
	viewMeths = viewMeths.tail;
      }
    }
    best
  }
}
}

