/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$


import scala.collection.mutable.HashMap;
import scalac._;
import scalac.util._;
import scalac.ast._;
import scalac.ast.printer._;
import scalac.symtab._;
import Tree._;
import scalac.{Global => scalac_Global}

package scala.tools.scalac.typechecker {

import scalac.util.NewArray;

/** Post-attribution checking and transformation.
 *
 *  This phase performs the following checks.
 *
 *   - All overrides conform to rules.
 *   - All type arguments conform to bounds.
 *   - All type variable uses conform to variance annotations.
 *   - No forward reference to a term symbol extends beyond a value definition.
 *
 *  It preforms the following transformations.
 *
 *   - Local modules are replaced by variables and classes
 *   - caseArity, caseElement implementations added to case classes
 *   - equals, and hashCode and toString methods are added to case classes,
 *     unless they are defined in the class or a baseclass
 *     different from java.lang.Object
 *   - Calls to case factory methods are replaced by new's.
 *   - Type nodes are replaced by TypeTerm nodes.
 *   - Eliminate constant definitions
 */
class RefCheck(globl: scalac.Global) extends Transformer(globl) {

  import Modifiers._;
  import Kinds._;

  private val defs: Definitions = globl.definitions;
  private val infer: Infer = globl.newInfer();

  private var unit: CompilationUnit = _;
  private var enclClass: Symbol = _;

// Forward reference checking ---------------------------------------------------

  private var levels: LevelInfo = _;
  private var symIndex = new HashMap[Symbol, int];

// Main entry point --------------------------------------------------------------

  override def apply(unit: CompilationUnit): unit = {
    this.unit = unit;
    levels = null;
    unit.body = transformStats(unit.body);
    symIndex.clear;
  }

// Override checking ------------------------------------------------------------

  private val HIBOUND = 0;
  private val LOBOUND = 1;
  private val VUBOUND = 2;

  private def isIncomplete(sym: Symbol): boolean =
    sym.isDeferred() ||
    sym.isAbstractOverride() &&
    isIncomplete(sym.overriddenSymbol(sym.owner().parents()(0)));

  /** 1. Check all members of class `clazz' for overriding conditions.
   *  2. Check that only abstract classes have deferred members*
   *  3. Check that every member with an `override' modifier
   *     overrides a concrete member.
   */
  private def checkAllOverrides(pos: int, clazz: Symbol): unit = {
    val closure = clazz.closure();
    var i = 0; while (i < closure.length) {
      val basetype = closure(i);
      val baseclazz = basetype.symbol();
      val it = basetype.members().iterator(true);
      while (it.hasNext()) {
	val sym = it.next();
	if (sym.owner() == baseclazz) checkOverride(pos, clazz, sym);
      }
      i = i + 1
    }

    val parents = clazz.info().parents();
    val it = clazz.members().iterator(true);
    while (it.hasNext()) {
      val sym = it.next();
      if ((sym.flags & OVERRIDE) != 0 && sym.owner() == clazz) {
	var i = parents.length - 1;
	while (i >= 0 && sym.overriddenSymbol(parents(i)).kind == NONE)
	  i = i - 1;
	if (i < 0) {
	  unit.error(sym.pos, sym.toString() + ":" + sym.getType() +
                     sym.locationString() + " overrides nothing");
	  sym.flags = sym.flags & ~OVERRIDE;
	} else if (sym.isAbstractOverride() && sym.overriddenSymbol(parents(0)).kind == NONE) {
	  unit.error(sym.pos,
		     sym.toString() + " does not override a superclass member in " +
		     parents(0));
	}
      }
    }
  }

  private def checkOverride(pos: int, clazz: Symbol, other: Symbol): unit = {

    def abstractClassError(msg: String) = {
      if (clazz.isAnonymousClass())
	unit.error(clazz.pos, "object creation impossible, since " + msg);
      else
	unit.error(clazz.pos,
	           clazz.toString() + " needs to be abstract, since " + msg);
      clazz.flags = clazz.flags | ABSTRACT;
    }

    if (other.kind == CLASS)
      return; // todo: see if we can sustain this
    var member = other;
    if ((other.flags & PRIVATE) == 0) {
      val member1 = clazz.info().lookup(other.name);
      if (member1.kind != NONE && member1.owner() != other.owner()) {
	if (member1.kind == VAL) {
	  val self = clazz.thisType();
	  val otherinfo = normalizedInfo(self, other);
	  member1.info() match {
	    case Type$OverloadedType(alts, _) =>
	      var i = 0; while (i < alts.length) {
		if (normalizedInfo(self, alts(i)).overrides(otherinfo)) {
		  if (member == other)
		    member = alts(i);
		  else
		    unit.error(
		      pos,
		      "ambiguous override: both " +
		      member + ":" + normalizedInfo(self, member) +
		      "\n and " + alts(i) + ":" + normalizedInfo(self, alts(i)) +
		      "\n override " + other + ":" + otherinfo +
		      other.locationString());
		}
                i = i + 1
	      }

            case _ =>
	      if (normalizedInfo(self, member1).overrides(otherinfo))
		member = member1;
          }
	} else member = member1;
      }
      if (member != other) {
	member.flags = member.flags | ACCESSED;
	checkOverride(pos, clazz, member, other);
      }
    }
    if (clazz.kind == CLASS && (clazz.flags & ABSTRACT) == 0) {
      if ((member.flags & DEFERRED) != 0) {
	val parents = clazz.parents();
	var i = 0; while (i < parents.length) {
	  val p = parents(i).symbol();
	  if (p.isSubClass(member.owner()) && (p.flags & ABSTRACT) == 0)
	    return; // everything was already checked elsewhere
          i = i + 1
	}
	abstractClassError(
	  member.toString() + member.locationString() + " is not defined" +
	  (if ((member.flags & MUTABLE) == 0) ""
           else "\n(Note that variables need to be initialized to be defined)"));
      } else if (member.isAbstractOverride()) {
	val superclazz: Type = clazz.parents()(0);
	val sup = member.overriddenSymbol(superclazz, clazz);
	if (clazz.kind == CLASS && (clazz.flags & ABSTRACT) == 0 && isIncomplete(sup))
	  abstractClassError(
	    member.toString() + member.locationString() +
	    " is marked `abstract' and `override' and overrides an incomplete superclass member in " + superclazz);
      }
    }
  }

  /** Check that all conditions for overriding `other' by `member' are met.
   *  That is for overriding member M and overridden member O:
   *
   *    1. M must have the same or stronger access privileges as O.
   *    2. O must not be final.
   *    3. O is deferred, or M has `override' modifier.
   *    4. O is not a class, nor a class constructor.
   *    5. If O is a type alias, then M is an alias of O.
   *    6. If O is an abstract type then
   *         either M is an abstract type, and M's bounds are sharper than O's bounds.
   *         or M is a type alias or class which conforms to O's bounds.
   *    7. If O and M are values, then M's type is a subtype of O's type.
   *    8. If O is an immutable value, then so is M.
   *    9. If O is a member of the static superclass of the class in which
   *       M is defined, and O is labelled `abstract override', then
   *       M must be labelled `abstract override'.
   */
  private def checkOverride(_pos: int, clazz: Symbol, member: Symbol, other: Symbol): unit = {

    var pos = _pos;

    def overrideError(msg: String): unit = {
      if (!other.getType().isError() && !member.getType().isError())
	unit.error(pos,
		   "error overriding " + other + other.locationString() +
		   ";\n " + member + member.locationString() + " " + msg);
    }

    //System.out.println(member + ":" + member.getType() + member.locationString() + " overrides " + other + ":" + other.getType() + other.locationString() + " in " + clazz);//DEBUG

    if (member.owner() == clazz) pos = member.pos;
    else {
      if (member.owner().isSubClass(other.owner()) &&
	  (member.flags & DEFERRED) >= (other.flags & DEFERRED)) {
	    //System.out.println(member + member.locationString() + " shadows1 " + other + other.locationString() + " in " + clazz);//DEBUG
	    return; // everything was already checked elsewhere
          }

      val parents = clazz.parents();
      var memberShadowsOther = true;
      var i = 0;
      while (i < parents.length && memberShadowsOther) {
	val p = parents(i).symbol();
	val isSubMember = p.isSubClass(other.owner());
	val isSubOther = p.isSubClass(member.owner());
	if (isSubMember != isSubOther) {
	  memberShadowsOther = false;
	} else if (isSubMember && isSubOther &&
		   (member.flags & DEFERRED) >= (other.flags & DEFERRED)) {
		     //System.out.println(member + member.locationString() + " shadows2 " + other + other.locationString() + " in " + clazz);//DEBUG
		     return; // everything was already checked elsewhere
		   }
        i = i + 1
      }
      if (memberShadowsOther) {
	//System.out.println(member + member.locationString() + " shadows " + other + other.locationString() + " in " + clazz);//DEBUG
	return; // everything was already checked elsewhere
      }
    }

    if ((member.flags & PRIVATE) != 0) {
      overrideError("has weaker access privileges; it should not be private");
    } else if ((member.flags & PROTECTED) != 0 && (other.flags & PROTECTED) == 0) {
      overrideError("has weaker access privileges; it should not be protected");
    } else if ((other.flags & FINAL) != 0) {
      overrideError("cannot override final member");
/*
    } else if (other.kind == CLASS) {
      overrideError("cannot override a class");
*/
    } else if ((other.flags & DEFERRED) == 0 && ((member.flags & OVERRIDE) == 0)) {
      overrideError("needs `override' modifier");
    } else if (other.isAbstractOverride() && !member.isAbstractOverride() && member.owner() == clazz && clazz.parents()(0).symbol().isSubClass(other.owner())) {
      overrideError("needs `abstract' and `override' modifiers");
    } else if (other.isStable() && !member.isStable()) {
      overrideError("needs to be an immutable value");
    } else if ((member.flags & DEFERRED) == 0 && (other.flags & DEFERRED) == 0 && member.owner() != clazz && !clazz.parents()(0).symbol().isSubClass(other.owner())) {
      unit.error(pos, "conflict between concrete members " +
		 member + member.locationString() + " and " +
		 other + other.locationString() +
		 ":\n both are inherited from mixin classes; " +
		 "\n an overriding definition in the current template is required");
    } else {
      val site: Type = clazz.thisType();

      def overrideTypeError(boundkind: int): unit = {

        def infoString(sym: Symbol, symtype: Type): String = sym.kind match {
	  case Kinds.ALIAS => ", which equals " + symtype
	  case Kinds.TYPE  => " bounded" + (if (boundkind == LOBOUND) " from below" else "") + " by " + symtype
	  case Kinds.VAL   => " of type " + symtype
	  case _     => ""
	}

        def oterror(memberInfo: Type, otherInfo: Type): unit = {
	  unit.error(pos,
		     member.toString() + member.locationString() +
		     infoString(member, memberInfo) +
		     "\n cannot override " + other + other.locationString() +
		     infoString(other, otherInfo));
	  Type.explainTypes(memberInfo, otherInfo);
        }

	if (!other.getType().isError() && !member.getType().isError()) {
	  if (boundkind == LOBOUND) {
            oterror(site.memberLoBound(member), site.memberLoBound(other))
	  } else if (boundkind == VUBOUND) {
	    oterror(site.memberVuBound(member), site.memberVuBound(other));
	  } else {
	    oterror(normalizedInfo(site, member), normalizedInfo(site, other))
	  }
        }
      }

      other.kind match {
	case ALIAS =>
	  if (member.typeParams().length != 0)
	    overrideError("may not be parameterized");
	  if (other.typeParams().length != 0)
	    overrideError("may not override parameterized type");
	  if (!site.memberType(member).isSameAs(site.memberType(other)))
	    overrideTypeError(HIBOUND);

	case TYPE =>
	  if (member.typeParams().length != 0)
	    overrideError("may not be parameterized");
	  if (!site.memberInfo(member).isSubType(site.memberInfo(other)))
	    overrideTypeError(HIBOUND);
	  if (!site.memberLoBound(other).isSubType(site.memberLoBound(member)))
	    overrideTypeError(LOBOUND);
	  if (!site.memberVuBound(member).isSubType(site.memberVuBound(other)))
	    overrideTypeError(VUBOUND);

	case _ =>
	  if (other.isConstructor())
	    overrideError("cannot override a class constructor");
	  if (!normalizedInfo(site, member).isSubType(normalizedInfo(site, other)))
	    overrideTypeError(HIBOUND);
      }
    }
  }

  private def normalizedInfo(site: Type, sym: Symbol): Type =
    site.memberInfo(sym).derefDef();

  /** compensate for renaming during addition of access functions
  */
  private def normalizeName(name: Name): String = {
    val string = name.toString();
    if (string.endsWith("$")) string.substring(0, string.length() - 1)
    else string;
  }

// Basetype Checking --------------------------------------------------------

  /** 1. Check that only traits are inherited several times (except if the
   *     inheriting instance is a compund type).
   *  2. Check that later type instances in the base-type sequence
   *     of a class are subtypes of earlier type instances of the same trait.
   *  3. Check that case classes do not inherit from case classes.
   *  4. Check that at most one base type is a case-class.
   */
  private def validateBaseTypes(clazz: Symbol): unit = {

    val seen = new Array[Type](clazz.closure().length);

    def validateTypes(tps: Array[Type], caseSeen: Symbol, start: int): unit = {
      var i = tps.length - 1; while(i >= start) {
        validateType(tps(i).unalias(), caseSeen, if (i == 0) 0 else 1);
        i = i - 1
      }
    }

    def validateType(tp: Type, caseSeen: Symbol, start: int): unit = {
      var baseclazz = tp.symbol();
      if (baseclazz.kind == CLASS) {
        val index = clazz.closurePos(baseclazz);
        if (index < 0) return;
        if (seen(index) != null) {
          // check that only traits are inherited several times.
          if (!clazz.isCompoundSym() && !baseclazz.isTrait()) {
            unit.error(clazz.pos, "illegal inheritance;\n" + clazz +
                       " inherits " + baseclazz + " twice");
          }
          // if there are two different type instances of same class
          // check that second is a subtype of first.
          if (!seen(index).isSubType(tp)) {
            val msg =
              if (clazz.isCompoundSym()) "illegal combination;\n compound type combines"
              else "illegal inheritance;\n " + clazz + " inherits";
            unit.error(clazz.pos, msg + " different type instances of " +
                       baseclazz + ":\n" + tp + " and " + seen(index));
          }
        }
        // check that case classes do not inherit from case classes
	var caseSeen1 =
          if (baseclazz.isCaseClass()) {
            if (caseSeen.isCaseClass())
              unit.error(
                clazz.pos, "illegal combination of case " +
                caseSeen + " and case " + baseclazz + " in one object");
            baseclazz
          } else caseSeen;

        seen(index) = tp;
        validateTypes(tp.parents(), caseSeen1, start);
      }
    }

    validateTypes(clazz.getType().parents(), clazz, 0);
  }

// Variance Checking --------------------------------------------------------

  private val ContraVariance = -1;
  private val NoVariance = 0;
  private val CoVariance = 1;
  private val AnyVariance = 2;

  private def varianceString(variance: int): String =
    if (variance == 1) "covariant"
    else if (variance == -1) "contravariant"
    else "invariant";

  /** The variance of symbol `base' relative to the class which defines `tvar'.
   */
  private def flip(base: Symbol, tvar: Symbol): int = {
    val clazz = tvar.owner().constructorClass();
    var sym = base;
    var flip = CoVariance;
    while (sym != clazz && flip != AnyVariance) {
      //System.out.println("flip: " + sym + " " + sym.isParameter());//DEBUG
      if (sym.isParameter()) flip = -flip;
      else if (sym.owner().kind != CLASS) flip = AnyVariance;
      else if (sym.kind == ALIAS) flip = NoVariance;
      sym = sym.owner();
    }
    flip
  }

  /** Check variance of type variables in this type
   */
  private def validateVariance(base: Symbol, all: Type, variance: int): unit = {

    def validateVariance(tp: Type, variance: int): unit = tp match {
      case Type.ErrorType | Type.AnyType | Type.NoType | Type.NoPrefix | Type$ThisType(_) | Type$ConstantType(_, _) =>

      case Type$SingleType(pre, sym) =>
        validateVariance(pre, variance)

      case Type$TypeRef(pre, sym, args) =>
        if (sym.variance() != 0) {
          val f = flip(base, sym);
          if (f != AnyVariance && sym.variance() != f * variance) {
            //System.out.println("flip(" + base + "," + sym + ") = " + f);//DEBUG
            unit.error(base.pos,
                       varianceString(sym.variance()) + " " + sym +
                       " occurs in " + varianceString(f * variance) +
                       " position in type " + all + " of " + base);
          }
        }
        validateVariance(pre, variance);
        validateVarianceArgs(args, variance, sym.typeParams());

      case Type$CompoundType(parts, members) =>
        validateVariances(parts, variance);

      case Type$MethodType(vparams, result) =>
        validateVariance(result, variance);

      case Type$PolyType(tparams, result) =>
        validateVariance(result, variance);

      case Type$OverloadedType(alts, alttypes) =>
        validateVariances(alttypes, variance);

      case _ => throw new ApplicationError("unexpected type: " + tp);
    }

    def validateVariances(tps: Array[Type], variance: int): unit = {
      var i = 0; while (i < tps.length) {
	validateVariance(tps(i), variance);
        i = i + 1
      }
    }

    def validateVarianceArgs(tps: Array[Type], variance: int, tparams: Array[Symbol]): unit = {
      var i = 0; while (i < tps.length) {
	validateVariance(tps(i), variance * tparams(i).variance());
        i = i + 1
      }
    }

    validateVariance(all, variance)
  }


// Forward reference checking ---------------------------------------------------

  case class LevelInfo(outer: LevelInfo) {
    val scope: Scope = if (outer == null) new Scope() else new Scope(outer.scope);
    var maxindex: int = Integer.MIN_VALUE;
    var refpos: int = _;
    var refsym: Symbol = _;
  }

  private def pushLevel(): unit =
    levels = new LevelInfo(levels);

  private def popLevel(): unit =
    levels = levels.outer;

  private def enterSyms(stats: Array[Tree]): unit = {

    def enterSym(stat: Tree, index: int): unit = {

      def enterSym(sym: Symbol): unit =
	if (sym.isLocal()) {
	  levels.scope.enter(new Scope.Entry(sym, levels.scope));
	  symIndex.update(sym, index);
	}

      stat match {
        case Tree$ClassDef(_, _, _, _, _, _) =>
	  enterSym(stat.symbol().primaryConstructor())

        case DefDef(_, _, _, _, _, _) | ModuleDef(_, _, _, _) | ValDef(_, _, _, _) =>
	  enterSym(stat.symbol())

        case _ =>
      }
    }

    var i = 0; while (i < stats.length) {
      enterSym(stats(i), i);
      i = i + 1
    }
  }

// Module eliminiation -----------------------------------------------------------

  private def transformModule(tree: Tree, mods: int, name: Name, tpe: Tree, templ: Tree.Template): Array[Tree] = {
    val sym = tree.symbol();
    val cdef = gen.ClassDef(sym.moduleClass(), templ);
    if (sym.isStatic()) return NewArray.Tree{cdef};
    val alloc =
      gen.New(
        gen.mkApply__(
          gen.mkPrimaryConstructorLocalRef(tree.pos, sym.moduleClass())));

    // var m$: T = null;
    val varname = Name.fromString(name.toString() + "$");
    val mvar = sym.owner().newFieldOrVariable(
      tree.pos, PRIVATE | MUTABLE | SYNTHETIC, varname)
      .setInfo(sym.getType());
    sym.owner().members().enterOrOverload(mvar);
    val vdef = gen.ValDef(mvar, gen.mkNullLit(tree.pos));

    // { if (null == m$) m$ = new m$class; m$ }
    val eqMethod: Symbol = getUnaryMemberMethod(
      sym.getType(), Names.EQEQ, defs.ANY_TYPE());
    val body: Tree =
      gen.mkBlock(
        gen.If(
	  gen.Apply(
	    gen.Select(gen.mkNullLit(tree.pos), eqMethod),
	    NewArray.Tree{gen.mkLocalRef(tree.pos, mvar)}),
	  gen.Assign(gen.mkLocalRef(tree.pos, mvar), alloc),
	  gen.mkUnitLit(tree.pos)),
	gen.mkLocalRef(tree.pos, mvar));

    // def m: T = { if (m$ == null(T)) m$ = new m$class; m$ }
    sym.flags = sym.flags | STABLE;
    val ddef: Tree = gen.DefDef(sym, body);

    // def m_eq(m: T): Unit = { m$ = m }
    val m_eqname = Name.fromString(name.toString() + Names._EQ);
    val m_eq: Symbol = sym.owner().newMethodOrFunction(
      tree.pos, PRIVATE | SYNTHETIC, m_eqname);
    val m_eqarg: Symbol = m_eq.newVParam(tree.pos, SYNTHETIC, name, sym.getType());
    m_eq.setInfo(
      Type.MethodType(NewArray.Symbol{m_eqarg}, defs.void_TYPE()));
    val m_eqdef: Tree = gen.DefDef(
      m_eq,
      gen.Assign(gen.mkLocalRef(tree.pos, mvar), gen.Ident(tree.pos, m_eqarg)));
    sym.owner().members().enterOrOverload(m_eq);

    NewArray.Tree(cdef, vdef, ddef, m_eqdef)
  }

// Adding case methods --------------------------------------------------------------

  private def hasImplementation(clazz: Symbol, name: Name): boolean = {
    val sym = clazz.info().lookupNonPrivate(name);
    sym.kind == VAL &&
    (sym.owner() == clazz ||
     !defs.OBJECT_CLASS.isSubClass(sym.owner()) &&
     (sym.flags & DEFERRED) == 0);
  }

  private def getMember(site: Type, name: Name): Symbol = {
    val sym = site.lookupNonPrivate(name);
    if (sym.kind != VAL)
      throw new ApplicationError("getMember failed for " + Debug.show(sym) + "; " + Debug.show(site) + "::" + name);
    sym
  }

  private def getMethod(site: Type, name: Name, p: Type => boolean): Symbol = {
    val sym = getMember(site, name);
    sym.getType() match {
      case Type$OverloadedType(alts, alttypes) =>
	var i = 0; while (i < alts.length) {
	  if (p(alttypes(i))) return alts(i);
          i = i + 1
	}
      case _ =>
    }
    if (p(sym.getType())) return sym;
    throw new ApplicationError(
      " no method " + name + " of required kind among " + sym.getType() + " at " + site);
  }

  private def getNullaryMemberMethod(site: Type, name: Name): Symbol =
    getMethod(site, name,
              tp => tp.paramSectionCount() == 1 && tp.firstParams().length == 0);

  private def getUnaryMemberMethod(site: Type, name: Name, paramtype: Type): Symbol =
    getMethod(site, name,
              tp => { val params = tp.firstParams();
                      params.length == 1 && paramtype.isSubType(params(0).getType()) });

  private def caseFields(clazz: ClassSymbol): Array[Tree] = {
    var ct = clazz.primaryConstructor().getType();
    ct match {
      case Type$PolyType(tparams, restp) =>
	ct = infer.skipViewParams(tparams, restp);
      case _ =>
    }
    val vparams: Array[Symbol] = ct.firstParams();
    val fields: Array[Tree] = new Array[Tree](vparams.length);
    var i = 0; while (i < fields.length) {
      fields(i) = gen.mkRef(clazz.pos, clazz.thisType(), clazz.caseFieldAccessor(i));
      i = i + 1
    }
    fields
  }

  private def toStringMethod(clazz: ClassSymbol): Tree = {
    val toStringSym = clazz.newMethod(
      clazz.pos, OVERRIDE, Names.toString)
      .setInfo(defs.ANY_TOSTRING.getType());
    clazz.info().members().enter(toStringSym);
    val fields: Array[Tree] = caseFields(clazz);
    var body: Tree = null;
    if (fields.length == 0) {
      body = gen.mkStringLit(
	clazz.pos, NameTransformer.decode(clazz.name));
    } else {
      body = gen.mkStringLit(
	clazz.pos, NameTransformer.decode(clazz.name) + "(");
      var i = 0; while (i < fields.length) {
	val str = if (i == fields.length - 1) ")" else ",";
	body = gen.Apply(
	  gen.Select(body, defs.STRING_PLUS), NewArray.Tree(fields(i)));
	body = gen.Apply(
	  gen.Select(body, defs.STRING_PLUS), NewArray.Tree(gen.mkStringLit(clazz.pos, str)));
        i = i + 1
      }
    }
    gen.DefDef(toStringSym, body)
  }

  private def caseElementMethod(clazz: ClassSymbol): Tree = {
    val seSym =
      clazz.newMethod( clazz.pos, FINAL|OVERRIDE, Names.caseElement );
    val seParam =
      seSym.newVParam( clazz.pos, 0, Names.n, defs.int_TYPE() );
    seSym.setInfo(
      Type.MethodType( NewArray.Symbol(seParam), defs.ANY_TYPE() ));
    clazz.info().members().enter( seSym );
    val fields: Array[Tree] = caseFields( clazz );
    var body: Tree = null;
    if (fields.length > 0) {        // switch< n >
      val tags: Array[int] = new Array[int](fields.length);
      var i = 0; while( i < fields.length ) { tags(i) = i; i = i + 1 }
      body = gen.Switch(
        gen.mkLocalRef(clazz.pos, seParam),
        tags,
        fields,
        gen.mkNullLit(clazz.pos),
        defs.ANY_TYPE());
    } else
      body = gen.mkNullLit( clazz.pos );
    gen.DefDef(seSym, body)
  }

  private def caseArityMethod(clazz: ClassSymbol): Tree = {
    val seSym =
      clazz.newMethod( clazz.pos, FINAL|OVERRIDE, Names.caseArity );
    seSym.setInfo(
      Type.PolyType( Symbol.EMPTY_ARRAY, defs.int_TYPE() ));
    clazz.info().members().enter( seSym );
    val fields: Array[Tree] = caseFields( clazz );
    gen.DefDef(seSym, gen.mkIntLit( clazz.pos, fields.length ))
  }


  private def equalsMethod(clazz: ClassSymbol): Tree = {
    val equalsSym = clazz.newMethod(clazz.pos, OVERRIDE, Names.equals);
    val equalsParam = equalsSym.newVParam(
      clazz.pos, 0, Names.that, defs.ANY_TYPE());
    equalsSym.setInfo(
      Type.MethodType(NewArray.Symbol(equalsParam), defs.boolean_TYPE()));
    clazz.info().members().enter(equalsSym);
    val fields = caseFields(clazz);
    var testtp = clazz.getType();
    {
      val tparams = clazz.typeParams();
      if (tparams.length != 0) {
	val targs = new Array[Type](tparams.length);
	var i = 0; while (i < targs.length) {
	  targs(i) = defs.ANY_TYPE();
          i = i + 1
        }
        testtp = testtp.subst(tparams, targs);
      }
    }

    // if (that is C) {...}
    val cond = gen.TypeApply(
      gen.Select(gen.mkLocalRef(clazz.pos, equalsParam), defs.ANY_IS),
      NewArray.Tree(gen.mkType(clazz.pos, testtp)));

    val thenpart =
      if (fields.length == 0) {
	gen.mkBooleanLit(clazz.pos, true)
      } else {
	// val that1 = that.asInstanceOf[C];
	val cast =
          gen.TypeApply(
	    gen.Select(
	      gen.mkLocalRef(clazz.pos, equalsParam),
	      defs.ANY_AS),
	    NewArray.Tree(gen.mkType(clazz.pos, testtp)));
	val that1sym = equalsSym.newVariable(clazz.pos, 0, Names.that1)
	  .setType(testtp);
	val that1def = gen.ValDef(that1sym, cast);

	def eqOp(l: Tree, r: Tree): Tree = {
	  val eqMethod = getUnaryMemberMethod(l.getType(), Names.EQEQ, r.getType());
	  gen.Apply(gen.Select(l, eqMethod), NewArray.Tree(r))
	}

        def qualCaseField(qual: Tree, i: int): Tree =
	  gen.Select(qual, clazz.caseFieldAccessor(i));

	// this.elem_1 == that1.elem_1 && ... && this.elem_n == that1.elem_n
	var cmp: Tree =
          eqOp(
	    fields(0),
	    qualCaseField(gen.mkLocalRef(clazz.pos, that1sym), 0));
        var i = 1; while (i < fields.length) {
	  cmp =
            gen.Apply(
	      gen.Select(cmp, defs.BOOLEAN_AND()),
	      NewArray.Tree(
		eqOp(
		  fields(i),
		  qualCaseField(gen.mkLocalRef(clazz.pos, that1sym), i))));
          i = i + 1
	}
	gen.mkBlock(that1def, cmp)
      }

    val body = gen.If(cond, thenpart, gen.mkBooleanLit(clazz.pos, false));

    gen.DefDef(equalsSym, body)
  }

  private def tagMethod(clazz: ClassSymbol): Tree = {
    val flags = if (clazz.isSubClass(defs.SCALAOBJECT_CLASS)) OVERRIDE else 0;
    val tagSym = clazz.newMethod(clazz.pos, flags, Names.tag)
      .setInfo(Type.MethodType(Symbol.EMPTY_ARRAY, defs.int_TYPE()));
    clazz.info().members().enter(tagSym);
    gen.DefDef(
      tagSym,
      gen.mkIntLit(clazz.pos, if (clazz.isCaseClass()) clazz.tag() else 0));
  }

  private def hashCodeMethod(clazz: ClassSymbol): Tree = {
    val hashCodeSym = clazz.newMethod(clazz.pos, OVERRIDE, Names.hashCode)
      .setInfo(defs.ANY_HASHCODE.getType());
    clazz.info().members().enter(hashCodeSym);
    val fields: Array[Tree] = caseFields(clazz);
    val name: Name = if (globl.target == scalac_Global.TARGET_MSIL) Names.GetType
                     else Names.getClass;
    val getClassMethod = getNullaryMemberMethod(clazz.getType(), name);
    val addMethod = getUnaryMemberMethod(defs.int_TYPE(), Names.ADD, defs.int_TYPE());
    val mulMethod = getUnaryMemberMethod(defs.int_TYPE(), Names.MUL, defs.int_TYPE());
    var body: Tree =
      gen.Apply(
	gen.Select(
	  gen.Apply(
	    gen.mkRef(clazz.pos, clazz.thisType(), getClassMethod),
	    Tree.EMPTY_ARRAY),
	  getNullaryMemberMethod(getClassMethod.getType().resultType(), Names.hashCode)),
	Tree.EMPTY_ARRAY);
    var i = 0; while (i < fields.length) {
      val operand: Tree =
        gen.Apply(
	  gen.Select(
	    fields(i),
	    getNullaryMemberMethod(fields(i).getType(), Names.hashCode)),
	  Tree.EMPTY_ARRAY);
      body =
	gen.Apply(
	  gen.Select(
	    gen.Apply(
	      gen.Select(body, mulMethod),
	      NewArray.Tree(gen.mkIntLit(clazz.pos, 41))),
	    addMethod),
	  NewArray.Tree(operand));
      i = i + 1
    }
    gen.DefDef(hashCodeSym, body)
  }

  private def addCaseMethods(templ: Template, clazz: ClassSymbol): Template = {
    val ts = new TreeList();
    if (clazz.isCaseClass()) {
      if (!hasImplementation(clazz, Names.toString)) {
	ts.append(toStringMethod(clazz));
      }
      if (!hasImplementation(clazz, Names.equals))
	ts.append(equalsMethod(clazz));
      if (!hasImplementation(clazz, Names.hashCode))
	ts.append(hashCodeMethod(clazz));

      // the following report error if impl exists
      ts.append(caseElementMethod(clazz));
      ts.append(caseArityMethod(clazz));
      ts.append(tagMethod(clazz));
    } else if ((clazz.flags & ABSTRACT) == 0) {
      ts.append(tagMethod(clazz));
    }
    copy.Template(
      templ, templ.parents,
      if (ts.length() > 0) {
	val body1 = new Array[Tree](templ.body.length + ts.length());
	System.arraycopy(templ.body, 0, body1, 0, templ.body.length);
	ts.copyTo(body1, templ.body.length);
	body1
      } else templ.body);
  }

// Convert case factory calls to constructor calls ---------------------------

  /** Tree represents an application of a constructor method of a case class
   *  (whose name is a term name). Convert this tree to application of
   *  the case classe's primary constructor `constr'.
   */
  private def toConstructor(tree: Tree, constr: Symbol): Tree = {

    def applyNesting(tree: Tree): int = tree match {
      case Apply(fn, _) => applyNesting(fn) + 1
      case _ => 0
    }

    val missing: int = constr.getType().paramSectionCount() - applyNesting(tree);
    assert(missing >= 0 && missing <= 1);

    def addEmptyParams(tp: Type): Type = tp match {
      case Type$MethodType(vparams, restp) =>
	Type.MethodType(vparams, addEmptyParams(restp))
      case Type$PolyType(tparams, restp) =>
	Type.PolyType(tparams, addEmptyParams(restp))
      case _ =>
	Type.MethodType(Symbol.EMPTY_ARRAY, tp)
    }

    def toConstructor1(tree: Tree): Tree = {
      val tree1 = toConstructor2(tree);
      if (missing > 0)
	tree1.duplicate().setType(addEmptyParams(tree1.getType()))
      else tree1
    }

    def toConstructor2(tree: Tree): Tree = tree match {
      case Tree$Apply(fn, args) =>
	copy.Apply(tree, toConstructor1(fn), args)
      case Tree$TypeApply(fn, args) =>
	copy.TypeApply(tree, toConstructor1(fn), args)
      case Tree$Ident(_) =>
	copy.Ident(tree, constr)
      case Tree$Select(qual, _) =>
	copy.Select(tree, constr, qual)
    }

    val tree1 = toConstructor1(tree);
    if (missing > 0) gen.Apply(tree1, Tree.EMPTY_ARRAY) else tree1
  }
    //where

// Bounds checking -----------------------------------------------------------

  private def checkBounds(pos: int, tparams: Array[Symbol], argtypes: Array[Type]): unit =
    if (tparams.length == argtypes.length) {
      try {
	infer.checkBounds(tparams, argtypes, "");
      } catch {
        case ex: Type.Error => unit.error(pos, ex.msg);
      }
    }

// Tree node simplification---------------------------------------------------

  private def elimTypeNode(tree: Tree): Tree =
    if (tree.isType()) gen.mkType(tree.pos, tree.getType().deconst())
    else tree;

// Transformation ------------------------------------------------------------

  def transformStats(stats: Array[Tree]): Array[Tree] = {
    pushLevel();
    enterSyms(stats);
    var stats1 = stats;
    var i = 0;
    while (i < stats1.length) {
      transformStat(stats1(i), i) match {
        case newstat: Tree =>
	  stats1(i) = newstat;
	  i = i + 1;
        case newstats: Array[Tree] =>
	  val stats2 = new Array[Tree](stats1.length - 1 + newstats.length);
	  System.arraycopy(stats1, 0, stats2, 0, i);
	  System.arraycopy(newstats, 0, stats2, i, newstats.length);
	  System.arraycopy(stats1, i + 1, stats2, i + newstats.length,
			   stats1.length - i - 1);
	  stats1 = stats2;
	  i = i + newstats.length;
        }
    }
    popLevel();
    stats1
  }

  def transformStat(tree: Tree, index: int): Any = tree match {
    case Tree$ModuleDef(mods, name, tpe, templ) =>
      transform(transformModule(tree, mods, name, tpe, templ))

    case Tree$ValDef(mods, name, tpe, rhs) =>
      val sym = tree.symbol();
      validateVariance(
	sym, sym.getType(),
	if ((sym.flags & MUTABLE) != 0) NoVariance else CoVariance);
      val tree1 = transform(tree);
      //todo: handle variables
      if (sym.isLocal() && !sym.isModule() && index <= levels.maxindex) {
	if (globl.debug)
	  System.out.println(levels.refsym.toString() + ":" + levels.refsym.getType());
	val kind = if ((sym.flags & MUTABLE) != 0) "variable" else "value";
	unit.error(
	  levels.refpos,
	  "forward reference extends over definition of " + kind + " " +
	  normalizeName(name));
      }
      tree1

    case _ =>
      transform(tree)
  }

  override def transform(tree: Tree): Tree = {
    val sym = tree.symbol();
    tree match {
      case Tree.Empty =>
        tree

      case Tree$ClassDef(_, _, tparams, vparams, tpe, templ) =>
	val enclClassPrev = enclClass;
        enclClass = sym;
        validateVariance(sym, sym.info(), CoVariance);
        validateVariance(sym, sym.typeOfThis(), CoVariance);
        val tree1 = super.transform(
	  copy.ClassDef(tree, tree.symbol(), tparams, vparams, tpe,
                        addCaseMethods(templ, tree.symbol().asInstanceOf[ClassSymbol])));
        enclClass = enclClassPrev;
        tree1

      case Tree$DefDef(_, _, _, _, _, _) =>
	validateVariance(sym, sym.getType(), CoVariance);
        super.transform(tree)

      case Tree$ValDef(_, _, _, _) =>
	validateVariance(
	  sym, sym.getType(),
	  if ((sym.flags & MUTABLE) != 0) NoVariance else CoVariance);
        super.transform(tree)

      case Tree$AbsTypeDef(_, _, _, _) =>
	validateVariance(sym, sym.info(), CoVariance);
        validateVariance(sym, sym.loBound(), ContraVariance);
        validateVariance(sym, sym.vuBound(), CoVariance);
        super.transform(tree)

      case Tree$AliasTypeDef(_, _, _, _) =>
	validateVariance(sym, sym.info(), CoVariance);
        super.transform(tree)

      case Tree$Template(bases, body) =>
	val bases1 = transform(bases);
        val body1 = transformStats(body);
        if (sym.kind == VAL) {
	  val owner = tree.symbol().owner();
	  validateBaseTypes(owner);
	  checkAllOverrides(tree.pos, owner);
	}
        copy.Template(tree, bases1, body1)

      case Tree$Block(stats, value) =>
	val stats1 = transformStats(stats);
        val value1 = transform(value);
        copy.Block(tree, stats1, value1)

      case Tree$This(_) =>
	tree

      case Tree$PackageDef(pkg, packaged) =>
	copy.PackageDef(tree, pkg, super.transform(packaged))

      case Tree$TypeApply(fn, args) =>
	fn.getType() match {
          case Type$PolyType(tparams, _) =>
	    checkBounds(tree.pos, tparams, Tree.typeOf(args));
          case _ =>
	}
        super.transform(tree)

      case Tree$Apply(fn, args) =>
	// convert case methods to new's
	val fsym = TreeInfo.methSymbol(fn);
        assert(fsym != Symbol.NONE, tree);
        if (fsym != null && fsym.isMethod() && !fsym.isConstructor() &&	(fsym.flags & CASE) != 0) {
	  val constr = fsym.getType().resultType().symbol().primaryConstructor();
	  super.transform(gen.New(toConstructor(tree, constr)))
	} else
          super.transform(tree);

      case Tree$AppliedType(tpe, args) =>
	val tparams = tpe.getType().symbol().typeParams();
        checkBounds(tree.pos, tparams, Tree.typeOf(args));
        elimTypeNode(super.transform(tree))

      case Tree$CompoundType(_, _) =>
	val clazz = tree.getType().symbol();
        validateBaseTypes(clazz);
        checkAllOverrides(tree.pos, clazz);
        elimTypeNode(super.transform(tree))

      case Tree$Ident(name) =>
	if (name == TypeNames.WILDCARD_STAR)
	  tree
        else if( sym == defs.PATTERN_WILDCARD )
	  elimTypeNode(tree)
        else {
	  //System.out.println("name: "+name);
	  val e = levels.scope.lookupEntry(name);
	  //System.out.println("sym: "+sym);
	  if (sym.isLocal() && sym == e.sym) {
            var l = levels;
            while (l.scope != e.owner) l = l.outer;
	    val symindex = symIndex(tree.symbol());
	    if (l.maxindex < symindex) {
	      l.refpos = tree.pos;
	      l.refsym = e.sym;
	      l.maxindex = symindex;
	    }
	  }
	  sym.flags = sym.flags | ACCESSED;
	  elimTypeNode(tree)
        }

      case Tree$Select(qual, name) =>
	sym.flags = sym.flags | ACCESSED;
        if (!TreeInfo.isSelf(qual, enclClass))
	  sym.flags = sym.flags | SELECTOR;
        if ((sym.flags & DEFERRED) != 0) {
          qual match {
	    case Tree$Super(qualifier, mixin) =>
	      val sym1 = enclClass.thisSym().info().lookup(sym.name);
	      if (mixin != TypeNames.EMPTY || !isIncomplete(sym1))
		unit.error(
		  tree.pos,
		  "symbol accessed from super may not be abstract");
            case _ =>
	  }
	}
        elimTypeNode(super.transform(tree))

      case _ =>
	elimTypeNode(super.transform(tree))
    }
  }

  override def transform(trees: Array[Tree]): Array[Tree] =
    super.transform(trees);

  override def transform(trees: Array[Tree$CaseDef]): Array[Tree$CaseDef] =
    super.transform(trees);

  override def transform(trees: Array[Tree$AbsTypeDef]): Array[Tree$AbsTypeDef] =
    super.transform(trees);

  override def transform(trees: Array[Tree$ValDef]): Array[Tree$ValDef] =
    super.transform(trees);

  override def transform(trees: Array[Array[Tree$ValDef]]): Array[Array[Tree$ValDef]] =
    super.transform(trees);

}}

