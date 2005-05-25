/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

import scalac.{CompilationUnit, ApplicationError};
import scalac.{Global => scalac_Global};
import scalac.util._;
import scalac.ast._;
import scalac.symtab._;
import Tree._;
import scalac.transformer.OwnerTransformer;

package scala.tools.scalac.transformer {

import scalac.util.NewArray;
/*<export>*/
/** - uncurry all symbol and tree types (@see UnCurryPhase)
 *  - for every curried parameter list:  (ps_1) ... (ps_n) ==> (ps_1, ..., ps_n)
 *  - for every curried application: f(args_1)...(args_n) ==> f(args_1, ..., args_n)
 *  - for every type application: f[Ts] ==> f[Ts]() unless followed by parameters
 *  - for every use of a parameterless function: f ==> f()  and  q.f ==> q.f()
 *  - for every def-parameter:  def x: T ==> x: () => T
 *  - for every use of a def-parameter: x ==> x.apply()
 *  - for every argument to a def parameter `def x: T':
 *      if argument is not a reference to a def parameter:
 *        convert argument `e' to (expansion of) `() => e'
 *  - for every argument list that corresponds to a repeated parameter
 *       (a_1, ..., a_n) => (Sequence(a_1, ..., a_n))
 *  - for every argument list that is an escaped sequence
 *       (a_1:_*) => (a_1)
 */
/*</export>*/
class UnCurry(global: scalac_Global, descr: UnCurryPhase) extends OwnerTransformer(global) {

  import Modifiers._;

  private var unit: CompilationUnit = _;
  private var inPattern: boolean = false;
  private var inArray: boolean = false;

  override def apply(unit: CompilationUnit): unit = {
    this.unit = unit;
    super.apply(unit);
  }

  /** (ps_1) ... (ps_n) => (ps_1, ..., ps_n)
   */
  private def uncurry(params: Array[Array[ValDef]]): Array[Array[ValDef]] = {
    var n = 0;
    { var i = 0; while (i < params.length) {
      n = n + params(i).length;
      i = i + 1
    }}
    val ps = new Array[ValDef](n);
    var j = 0;
    var i = 0; while (i < params.length) {
      System.arraycopy(params(i), 0, ps, j, params(i).length);
      j = j + params(i).length;
      i = i + 1
    }
    NewArray.ValDefArray(ps)
  }

  /** tree of non-method type T ==> same tree with method type ()T
  */
  private def asMethod(tree: Tree): Tree = tree.getType() match {
    case Type$MethodType(_, _) =>
      tree
    case _ =>
      tree.setType(
	new Type.MethodType(Symbol.EMPTY_ARRAY, tree.getType().widen()))
  }

  /** apply parameterless functions and def parameters
  */
  private def applyDef(tree: Tree): Tree = {
    assert(tree.symbol() != null, tree);
    tree.symbol().getType() match {
      case Type$PolyType(tparams, restp) =>
	if (tparams.length > 0 || (restp.isInstanceOf[Type$MethodType])) tree
	else gen.Apply(asMethod(tree), Tree.EMPTY_ARRAY)
      case _ =>
	if (tree.symbol().isDefParameter()) {
	  tree.setType(global.definitions.FUNCTION_TYPE(
	    Type.EMPTY_ARRAY, tree.getType().widen()));
	  gen.Apply(gen.Select(tree, global.definitions.FUNCTION_APPLY(0)));
	} else tree
    }
  }

  /** - uncurry all symbol and tree types (@see UnCurryPhase)
   *  - for every curried parameter list:  (ps_1) ... (ps_n) ==> (ps_1, ..., ps_n)
   *  - for every curried application: f(args_1)...(args_n) ==> f(args_1, ..., args_n)
   *  - for every type application: f[Ts] ==> f[Ts]() unless followed by parameters
   *  - for every use of a parameterless function: f ==> f()  and  q.f ==> q.f()
   *  - for every def-parameter:  def x: T ==> x: () => T
   *  - for every use of a def-parameter: x ==> x.apply()
   *  - for every argument to a def parameter `def x: T':
   *      if argument is not a reference to a def parameter =>
   *        convert argument `e' to (expansion of) `() => e'
   *  - for every argument list that corresponds to a repeated parameter
   *       (a_1, ..., a_n) => (Sequence(a_1, ..., a_n))
   */
  override def transform(tree: Tree): Tree = {
    var prevtype: Type = tree.getType();
    if (prevtype != null) {
      if (prevtype.isInstanceOf[Type$OverloadedType]) {
	assert(tree.symbol() != null);
	prevtype = tree.symbol().removeInheritedOverloaded(prevtype);
      }
      tree.setType(descr.uncurry(prevtype));
    }
    tree match {
      case Tree$ClassDef(_, _, tparams, vparams, tpe, impl) =>
        val clazz: Symbol = tree.symbol();
        val elems = clazz.members().elements();
        var i = 0; while (i < elems.length) {
          checkNoDoubleDef(clazz, elems(i));
          i = i + 1
        }
        copy.ClassDef(
	  tree, clazz, tparams,
	  uncurry(transform(vparams, clazz)),
	  tpe,
	  transform(impl, clazz));

      case Tree$DefDef(_, _, tparams, vparams, tpe, rhs) =>
	val sym: Symbol = tree.symbol();
        if (descr.isUnaccessedConstant(sym)) gen.mkUnitLit(tree.pos)
        else {
	  val rhs1: Tree = transform(rhs, sym);
	  copy.DefDef(
	    tree, sym, tparams, uncurry(transform(vparams, sym)), tpe, rhs1)
        }

      case Tree$ValDef(_, _, tpe, rhs) =>
	val sym: Symbol = tree.symbol();
        if (descr.isUnaccessedConstant(sym)) gen.mkUnitLit(tree.pos)
        else if (sym.isDefParameter()) {
	  val newtype: Type = global.definitions.FUNCTION_TYPE(Type.EMPTY_ARRAY, tpe.getType());
	  val tpe1: Tree = gen.mkType(tpe.pos, newtype);
          copy.ValDef(tree, tpe1, rhs).setType(newtype)
	} else
	  super.transform(tree)

      case Tree$TypeApply(fn, args) =>
	val tree1: Tree = asMethod(super.transform(tree));
        gen.Apply(tree1, new Array[Tree](0))

      case Tree$Apply(fn, args) =>
	// f(x)(y) ==> f(x, y)
	// argument to parameterless function e => ( => e)
	val ftype: Type = fn.getType();
        val fn1: Tree = transform(fn);
        val myInArray: boolean =
          TreeInfo.methSymbol(fn1) == global.definitions.PREDEF_ARRAY();
        val old = inArray;
        inArray = myInArray; // a hack to communicate with toSequence.
        val args1 = transformArgs(tree.pos, args, ftype);
        inArray   = old;
        if (myInArray) {
          args1(0).getType().baseType(global.definitions.ARRAY_CLASS).match {
            case Type.NoType =>
              copy.Apply(tree,fn, Predef.Array[Tree]{args1(0)});
            case _      => fn1 match {
              case Tree$Apply(TypeApply(Select(fn2, _), targs), _) =>
                return gen.mkBlock(args1(0).pos, fn2, args1(0))
              case _ =>
                assert(false, "dead")
            }
          }
        }
          if (TreeInfo.methSymbol(fn1) == global.definitions.ANY_MATCH && !(args1(0).isInstanceOf[Tree.Visitor])) {
	  TreeInfo.methPart(fn1) match {
	    case Tree$Select(qual, name) =>
	      assert(name == Names._match);
	      gen.postfixApply(qual, args1(0), currentOwner)
	    case _ =>
	      throw new ApplicationError("illegal prefix for match: " + tree);
	  }
	} else {
	  fn1 match {
	    case Tree$Apply(fn2, args2) =>
	      val newargs = new Array[Tree](args1.length + args2.length);
	      System.arraycopy(args2, 0, newargs, 0, args2.length);
	      System.arraycopy(args1, 0, newargs, args2.length, args1.length);
	      copy.Apply(tree, fn2, newargs);
	    case _ =>
	      copy.Apply(tree, fn1, args1);
	  }
	}

      case Tree$Select(_, _) =>
	applyDef(super.transform(tree))

      case Tree$Ident(name) =>
	if (name == TypeNames.WILDCARD_STAR) {
	  unit.error(tree.pos, " argument does not correspond to `*'-parameter");
	  tree
	} else if (tree.symbol() == global.definitions.PATTERN_WILDCARD) {
	  tree
	} else {
	  applyDef(super.transform(tree))
	}

      case Tree$CaseDef(pat, guard, body) =>
        inPattern = true;
        val pat1: Tree = transform( pat );
        inPattern = false;
        val guard1: Tree = transform( guard );
        val body1: Tree = transform( body );
        copy.CaseDef(tree, pat1, guard1, body1)

      case _ =>
	super.transform(tree)
    }
  }

  /** Transform arguments `args' to method with type `methtype'.
   */
  private def transformArgs(pos: int, args: Array[Tree], methtype: Type): Array[Tree] = {
    methtype match {
      case Type$MethodType(params, _) =>
        val args0 =
          if (params.length > 0 && (params(params.length-1).flags & REPEATED) != 0)
	    toSequence(pos, params, args)
          else args;
        var args1 = args0;
        var i = 0; while (i < args0.length) {
	  val arg: Tree = args0(i);
	  val arg1: Tree = transformArg(arg, params(i));
	  if (arg1 != arg && args1 == args0) {
	    args1 = new Array[Tree](args0.length);
	    System.arraycopy(args0, 0, args1, 0, i);
	  }
	  args1(i) = arg1;
          i = i + 1
	}
        args1;
      case Type$PolyType(_, restp) =>
	transformArgs(pos, args, restp)
      case _ =>
	if (args.length == 0) args; // could be arguments of nullary case pattern
	else throw new ApplicationError(methtype);
    }
  }

  /** converts `a_1,...,a_n' to Seq(a_1,...,a_n)
   *  if a_n is an escaped sequence x:_*, takes care of escaping
   *
   *  precondition: params[params.length-1].flags & REPEATED != 0
   */
  private def toSequence(pos: int, params: Array[Symbol], args: Array[Tree]): Array[Tree] = {
    val result = new Array[Tree](params.length);
    System.arraycopy(args, 0, result, 0, params.length - 1);
    assert(args.length != params.length
	   || !(args(params.length-1).isInstanceOf[Tree.Sequence])
	   || TreeInfo.isSequenceValued(args(params.length-1)));
    if (args.length == params.length) {
      args(params.length-1) match {
        case Tree$Typed(arg, Ident(TypeNames.WILDCARD_STAR)) => // seq:_* escape
	  result(params.length-1) = arg;
	  return result;
        case _ =>
      }
    }
    var args1 = args;
    if (params.length != 1) {
      args1 = new Array[Tree](args.length - (params.length - 1));
      System.arraycopy(args, params.length - 1, args1, 0, args1.length);
    }
    val theType: Type = params(params.length-1).getType();
    result(params.length-1) =
      if (inPattern)
        make.Sequence(pos, args1).setType(theType)
      else if (inArray)
        gen.mkNewArray(pos, theType.typeArgs()(0), args1, currentOwner)
      else
        gen.mkNewList(pos, theType.typeArgs()(0), args1);
    result
  }

  /** for every argument to a def parameter `def x: T':
   *    if argument is not a reference to a def parameter =>
   *      convert argument `e' to (expansion of) `() => e'
   */
  private def transformArg(arg: Tree, formal: Symbol): Tree =
    if ((formal.flags & DEF) == 0)
      transform(arg)
    else {
      val sym: Symbol = arg.symbol();
      if (sym == null || (sym.flags & DEF) == 0)
        transform(
	  gen.mkUnitFunction(arg, descr.uncurry(arg.getType().widen()),
                             currentOwner, true))
      else {
	val arg1: Tree = transform(arg);
	arg1 match {
	  case Tree$Apply(Select(qual, name), args1) =>
	    assert(name == Names.apply && args1.length == 0);
	    qual
	  case _ =>
	    System.err.println(arg1);//debug
	    throw new ApplicationError();
	}
      }
    }

// Double Definition Checking -----------------------------------------------

  private def checkNoDoubleDef(clazz: Symbol, sym: Symbol): unit = {

    def checkNoDoubleDef(sym1: Symbol, sym2: Symbol, type1: Type, type2: Type): unit = {

      def conflictError(phase: String): unit = {
        if (sym1.owner() == clazz && sym2.owner() == clazz)
          unit.error(sym2.pos,
                     "Double declaration:\n" +
                     sym1 + ": " + type1 + " and\n" +
                     sym2 + ": " + type2 + " have same types after " + phase);
        else if (sym1.owner() == clazz)
          unit.error(sym1.pos,
                     "Accidental override:\n" +
                     sym1 + ": " + type1 + " has same type after " + phase + " as\n" +
                     sym2 + ": " + type2 + " which is inherited from " + sym2.owner());
        else if (sym2.owner() == clazz)
          unit.error(sym2.pos,
                     "Accidental override:\n" +
                     sym2 + ": " + type2 + " has same type after " + phase + " as\n" +
                     sym1 + ": " + type1 + " which is inherited from " + sym1.owner());
        else
          unit.error(clazz.pos,
                     "Inheritance conflict: inherited members\n" +
                     sym1 + ": " + type1 + sym1.locationString() + " and\n" +
                     sym2 + ": " + type2 + sym2.locationString() + " have same types after " + phase);
      }

      val newtype1: Type = descr.uncurry(type1);
      val newtype2: Type = descr.uncurry(type2);
      if (sym1.owner() != sym2.owner() &&
          (newtype1.overrides(newtype2) || newtype2.overrides(newtype1)))
        conflictError("uncurry")
      else if (erasureConflict(newtype1, newtype2))
        conflictError("erasure")
    }

    sym.getType() match {
      case Type$OverloadedType(alts, alttypes) =>
        var i = 0; while (i < alttypes.length) {
          var j = i + 1; while (j < alttypes.length) {
            checkNoDoubleDef(alts(i), alts(j), alttypes(i), alttypes(j));
            j = j + 1
          }
          i = i + 1
        }
      case _ =>
    }
  }

  private def erasureConflict(type1: Type, type2: Type): boolean = type1 match {
    case Type$PolyType(_, restype1) =>
      erasureConflict(restype1, type2)

    case Type$MethodType(params1, restype1) =>
      type2 match {
        case Type$PolyType(_, restype2) =>
          erasureConflict(type1, restype2)

        case Type$MethodType(params2, restype2) =>
          if (params1.length != params2.length) return false;
          var i = 0; while (i < params1.length) {
            if (!params1(i).nextInfo().erasure().isSameAs(
              params2(i).nextInfo().erasure())) return false;
            i = i + 1
          }
          restype1.erasure().isSameAs(restype2.erasure())

        case _ =>
          false
      }

    case _ =>
      type2 match {
        case Type$PolyType(_, _) | Type$MethodType(_, _) =>
          erasureConflict(type2, type1);
        case _ =>
          true
      }
  }

  override def transform(templ: Tree$Template, sym: Symbol): Tree$Template =
    super.transform(templ, sym);

  override def transform(tree: Tree, sym: Symbol): Tree =
    super.transform(tree, sym);

  override def transform(trees: Array[Array[Tree$ValDef]], sym: Symbol): Array[Array[Tree$ValDef]] =
    super.transform(trees, sym);


}
}
