/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */
import java.io._;
import java.util.ArrayList;
import java.lang.Object;

import scalac._;
import scalac.util._;
import scalac.symtab._;
import scalac.ast._;
import scalac.typechecker.Infer;
import scalac.{Global => scalac_Global, Unit => scalac_Unit}

import scala.tools.scalac.ast.printer.TextTreePrinter;
import scala.tools.scalac.util.NewArray;

package scala.tools.scalac.typechecker {

/** A transformer for removing syntactic sugar. This transformer does
 *  not need any type or symbol-table information.
 *
 *  @author     Martin Odersky
 *  @version    2.0
 */
class DeSugarize(make: TreeFactory, copy: TreeCopier, gen: TreeGen, infer: scala.tools.scalac.typechecker.Infer, global: scalac_Global) {

  import Kinds._, Modifiers._;
  import scalac.ast.TreeList;

  def this(analyzer: Analyzer, global: scalac_Global) =
    this(analyzer.make, analyzer.copy, analyzer.gen, analyzer.infer, global);

// Auxiliary definitions and functions -------------------------------------------

  /** introduce fresh variable of the form "ds$56"
  */
  def getvar(unit: scalac_Unit): Name = unit.fresh.newName("ds", '$');

  def setterName(name: Name): Name = Name.fromString(name.toString() + Names._EQ);

  def parameterName(i: int): Name = Name.fromString("x$" + i);

  def tupleSelectorName(i: int): Name = Name.fromString("_" + i);

  /** extract variables from a pattern */
  def getVariables(tree: Tree, vars: ArrayList): unit = tree match {
    case Tree$Ident(name) =>
      if (name.isVariable() && name != Names.PATTERN_WILDCARD) vars.add(name);

    case Tree$Typed(expr, _) =>
      getVariables(expr, vars);

    case Tree$Apply(fn, args) =>
      fn match {
	case Tree$Apply(_, _) => getVariables(fn, vars);
	case _ =>
      }
      for (val i <- Iterator.range(0, args.length))
	getVariables(args(i), vars);

    case Tree$Sequence(elems) =>
      for (val i <- Iterator.range(0, elems.length))
	getVariables(elems(i), vars);

    case Tree$Literal( _ ) =>

    case Tree$Bind(name, t) =>
      if (name.isVariable() && name != Names.PATTERN_WILDCARD) vars.add(name);
      getVariables(t, vars);

    case Tree$Alternative(ts) =>
      for (val i <- Iterator.range(0, ts.length))
	getVariables(ts(i), vars);
  }

// Transform functions -----------------------------------------------------

  /**  (T_1, ..., T_N) => T  ==>  scala.FunctionN[T_1, ..., T_N, +T]
  */
  def FunType(tree: Tree): Tree = tree match {
    case Tree$FunType(argtpes, restpe) =>
      mkFunType(tree.pos, argtpes, restpe)
  }

  def mkFunType(pos: int, argtpes: Array[Tree], restpe: Tree): Tree = {
      val types = new Array[Tree](argtpes.length + 1);
      System.arraycopy(argtpes, 0, types, 0, argtpes.length);
      types(argtpes.length) = restpe;
      make.AppliedType(
	pos,
	make.Select(
	  pos,
	  make.Ident(pos, Names.scala),
	  Name.fromString("Function" + argtpes.length).toTypeName()),
	types);
  }

  def mkTuple(pos: int, trees: Array[Tree]): Tree =
    if (trees.length == 0)
      gen.mkUnitLit(pos);
    else
      make.Apply(
	pos,
	make.Select(
	  pos,
	  make.Ident(pos, Names.scala),
	  Name.fromString("Tuple" + trees.length)),
	trees);

  /** Convert method to function type.
  */
  def meth2fun(tp: Type): Type = tp match {
    case Type$MethodType(params, restype) =>
      global.definitions.FUNCTION_TYPE(
	Symbol.getType(params), meth2fun(restype));
    case _ =>
      tp
  }

  /** If `pt' is a matching function type insert missing parameters
  *  in `vparams' from it, return: and result type,
  *  else return AnyType.
  */
  def preFunction(vparams: Array[Tree$ValDef], pt: Type): Type = pt match {
    case Type$TypeRef(pre, psym, ptargs)
      if (ptargs.length == vparams.length + 1 &&
          vparams.length < global.definitions.FUNCTION_CLASS.length &&
          psym == global.definitions.FUNCTION_CLASS(vparams.length)) =>

      def assignType(vparam: Tree$ValDef, pt: Type): unit =
	if (vparam.tpe == Tree.Empty && infer.isFullyDefined(pt))
	  vparam.tpe = gen.mkType(vparam.pos, pt);

      for (val i <- Iterator.range(0, vparams.length))
	assignType(vparams(i), ptargs(i));

      ptargs(vparams.length);

    case _ =>
      Type.AnyType
    }

  def isDefinedAtVisitor(tree: Tree): Tree = tree match {
    case Tree$Visitor(cases) =>
      val lastCase = cases(cases.length - 1);
      lastCase match {
	case Tree$CaseDef(Tree$Ident(name), Tree.Empty, expr) if (name.isVariable()) =>
	  make.Visitor(
	    tree.pos,
	    NewArray.CaseDef(
	      make.CaseDef(
		lastCase.pos,
		lastCase.pat.duplicate(),
		Tree.Empty,
		gen.mkBooleanLit(lastCase.body.pos, true))));
	case _ =>
	  val cases1 = new Array[Tree$CaseDef](cases.length + 1);
	  for (val i <- Iterator.range(0, cases.length)) {
	    cases(i) match {
	      case Tree$CaseDef(pat, guard, _) =>
		cases1(i) = make.CaseDef(
		  cases(i).pos,
		  pat.duplicate(),
		  guard.duplicate(),
		  gen.mkBooleanLit(tree.pos, true));
	    }
	  }
	  cases1(cases.length) = make.CaseDef(
	    tree.pos,
	    gen.Ident(tree.pos, global.definitions.PATTERN_WILDCARD),
	    Tree.Empty,
	    gen.mkBooleanLit(tree.pos, false));
	  make.Visitor(tree.pos, cases1);
      }
  }

  /** match => this.match
  *  match[targs] => this.match[targs]
  *  IMPORTANT: tree is already attributed and attributes need to be preserved.
  */
  def postMatch(tree: Tree, currentclazz: Symbol): Tree = tree match {
    case Tree$Ident(name) =>
      make.Select(
	tree.pos,
	gen.This(tree.pos, currentclazz),
	name).setSymbol(tree.symbol()).setType(tree.getType());
    case Tree$TypeApply(fn, args) =>
      copy.TypeApply(tree, postMatch(fn, currentclazz), args);
    case _ =>
      tree
  }

  /** { cases }   ==>   (x => x.match {cases})
    *  only called when match has to be added
  *  no type for parameter x
  */
  def Visitor(unit: scalac_Unit, tree: Tree): Tree = tree match {
    case Tree$Visitor(cases) =>
      val x: Name = getvar(unit);
      val param: Tree$ValDef = make.ValDef(
	tree.pos, PARAM, x, Tree.Empty, Tree.Empty);
      val xuse: Tree = make.Ident(tree.pos, x);
      // x.match {cases}
      val body: Tree = make.Apply(
	tree.pos,
	make.Select(tree.pos, xuse, Names.match),
	NewArray.Tree(tree));
      make.Function(tree.pos, NewArray.ValDef(param), body);
  }

  /** e = e'   ==>   e_=(e')
  */
  def Assign(pos: int, lhs: Tree, rhs: Tree): Tree = {
    val lhs1 = lhs match {
      case Tree$Ident(name) =>
	make.Ident(lhs.pos, setterName(name));
      case Tree$Select(qual, name) =>
	make.Select(lhs.pos, qual, setterName(name));
    }
    make.Apply(pos, lhs1, NewArray.Tree(rhs));
  }

  /** e(args) = e'  ==>  e.update(args ; e')
  */
  def Update(tree: Tree): Tree = tree match {
    case Tree$Assign(Tree$Apply(fn, args), rhs) =>
      // e.update
      val update: Tree = make.Select(fn.pos, fn, Names.update);
      val args1 = new Array[Tree](args.length + 1);
      System.arraycopy(args, 0, args1, 0, args.length);
      args1(args.length) = rhs;
      make.Apply(tree.pos, update, args1);
  }

  /** Remove the DocDef shell, if any.
  */
  def unboxDocDef(tree: Tree): Tree = tree match {
    case Tree$DocDef(_, definition) => definition
    case _ => tree
  }

  /** If "tree" is a DocDef, add its comment to "trees".
  */
  def boxDocDef(trees: Array[Tree], tree: Tree): Array[Tree] = {
    tree match {
      case Tree$DocDef(comment, _) =>
        for (val i <- Iterator.range(0, trees.length))
          trees(i) = make.DocDef(tree.pos, comment, trees(i))
      case _ =>
    }
    trees
  }

  /** expand pattern definitions and variable definitions in templates.
  */
  def Statements(unit: scalac_Unit, stats: Array[Tree], isLocal: boolean): Array[Tree] = {
    var change: boolean = false;
    var i = 0;
    while (i < stats.length && !change) {
      unboxDocDef(stats(i)) match {
	case Tree$PatDef(_, _, _) =>
	  change = true;
	case Tree$ValDef(_, _, _, _) =>
	  change = !isLocal;
	case _ =>
      }
      i = i + 1
    }
    if (change) {
      val ts: TreeList = new TreeList();
      for (val i <- Iterator.range(0, stats.length)) {
        val stat = unboxDocDef(stats(i));
	stat match {
	  case Tree$PatDef(_, _, _) =>
	    ts.append(boxDocDef(Statements(unit, this.PatDef(unit, stat), isLocal), stats(i)));
	  case Tree$ValDef(_, _, _, _) =>
	    if (isLocal) ts.append(stats(i))
	    else ts.append(boxDocDef(this.ValDef(stat), stats(i)));
	  case _ =>
	    ts.append(stats(i));
	}
      }
      ts.toArray()
    } else {
      stats;
    }
  }

  /** expands view-bounded class and method definitions
  */
  def Definition(tree: Tree) =
    tree match {
      case Tree$ClassDef(mods, name, tparams, vparams, tpe, templ) =>
	make.ClassDef(tree.pos, mods, name, tparams,
		      addViewParams(tparams, vparams), tpe, templ);
      case Tree$DefDef(mods, name, tparams, vparams, tpe, rhs) =>
	make.DefDef(tree.pos, mods, name, tparams,
		    addViewParams(tparams, vparams), tpe, rhs)

      case _ =>
	tree
    }

  def addViewParams(tparams: Array[Tree$AbsTypeDef], vparams: Array[Array[Tree$ValDef]]): Array[Array[Tree$ValDef]] = {
    var viewparams = new TreeList();
    var i = 0;
    while (i < tparams.length) {
      tparams(i) match {
	case Tree$AbsTypeDef(mods, tname, rhs, _) if (mods & VIEWBOUND) != 0 =>
	  viewparams.append(
	    make.ValDef(
	      tparams(i).pos,
	      PARAM | SYNTHETIC,
	      Names.view,
	      mkFunType(
		tparams(i).pos,
		NewArray.Tree(make.Ident(tparams(i).pos, tname)),
		rhs.duplicate()),
	      Tree.Empty));
	case _ =>
      }
      i = i + 1
    }
    if (viewparams.length() > 0) {
      val vparams1 = new Array[Array[Tree$ValDef]](vparams.length + 1);
      vparams1(0) = new Array[Tree$ValDef](viewparams.length());
      viewparams.copyTo(vparams1(0));
      System.arraycopy(vparams, 0, vparams1, 1, vparams.length);
      vparams1
    } else vparams
  }

  /** expands pattern definitions
  *  in case pattern is a simple (typed) identifier:
  *  val x = e     ==>  val x = e
  *  val x: T = e  ==>  val x: T = e
  *
  *  in case there are no variables in pattern
  *  val p = e  ==>  e.match (case p => ())
  *
  *  in case there is exactly one variable in pattern
  *  val x_1 = e.match (case p => (x_1))
  *
  *  in case there are more variables in pattern
  *  val p = e  ==>  private synthetic val t$ = e.match (case p => (x_1, ..., x_N))
  *                  val x_1 = t$._1
  *                  ...
  *                  val x_N = t$._N
  *
  */
  def PatDef(unit: scalac_Unit, tree: Tree): Array[Tree] = tree match {
    case Tree$PatDef(mods, Tree$Ident(name), rhs) =>
      // val x = e     ==>  val x = e
      NewArray.Tree(make.ValDef(tree.pos, mods, name, Tree.Empty, rhs))

    case Tree$PatDef(mods, Tree$Typed(Tree$Ident(name), tpe), rhs) =>
      // val x: T = e  ==> val x: T = e
	NewArray.Tree(make.ValDef(tree.pos, mods, name, tpe, rhs))

    case Tree$PatDef(mods, pat, rhs) =>
      val pos: int = tree.pos;
      val varlist: ArrayList = new ArrayList();
      getVariables(pat, varlist);
      val vars = new Array[Name](varlist.size());
      varlist.toArray(vars.asInstanceOf[Array[Object]]);

      // Tuple_N(x_1, ..., x_N)
      val vtree = new Array[Tree](vars.length);
      for (val i <- Iterator.range(0, vars.length)) {
	vtree(i) = make.Ident(pos, vars(i));
      }
      val tuple: Tree = if (vars.length == 1) vtree(0)
			else mkTuple(tree.pos, vtree);

      // e.match (case p => Tuple_N(x_1, ..., x_N))
      val cases = NewArray.CaseDef(make.CaseDef(pos, pat, Tree.Empty, tuple));
      val match: Tree = make.Apply(
	pos,
	make.Select(pos, rhs, Names.match),
	NewArray.Tree(make.Visitor(pos, cases)));

      if (vars.length == 0) {
	// e.match (case p => ())
	print(pat, "patdef", match);
	NewArray.Tree(match);
      } else if (vars.length == 1) {
	// val x_1 = e.match (case p => x_1)
	val valdef: Tree = make.ValDef(pos, mods, vars(0), Tree.Empty, match);
	print(pat, "patdef", valdef);
	NewArray.Tree(valdef)
      } else {
	// t$
	val vble: Name = getvar(unit);

	// private synthetic val t$ = e.match (case p => (x_1, ..., x_N))
	val res = new Array[Tree](vars.length + 1);
	res(0) = make.ValDef(pos, PRIVATE | SYNTHETIC, vble, Tree.Empty, match);
	for (val i <- Iterator.range(0, vars.length)) {
	  // val x_i = t$._i
	  res(i + 1) = make.ValDef(
	    pos, mods, vars(i), Tree.Empty,
	    make.Select(pos, make.Ident(pos, vble), tupleSelectorName(i + 1)));
	}
	print(pat, "patdef", new Tree$Block(res, gen.mkUnitLit(pos)));//debug
	res
      }
  }

  def ValDef(tree: Tree): Array[Tree] = tree match {
    case Tree$ValDef(mods, name, tpe, rhs) =>
      val valname: Name = Name.fromString("" + name + "$");
      val valdef1: Tree = copy.ValDef(
	tree, (mods & (DEFERRED | MUTABLE | CASEACCESSOR | MODUL)) | PRIVATE,
	valname, tpe, rhs).setType(null);
      var mods1: int = mods | ACCESSOR;
      if ((mods1 & MUTABLE) == 0) mods1 = mods1 | STABLE;
      val getter: Tree = make.DefDef(
	tree.pos, mods1, name,
	Tree.AbsTypeDef_EMPTY_ARRAY, Tree.ValDef_EMPTY_ARRAY_ARRAY,
	tpe.duplicate(),
	if ((mods & DEFERRED) != 0) Tree.Empty
	else make.Ident(tree.pos, valname));
      if ((mods1 & MUTABLE) == 0) {
	if ((mods1 & DEFERRED) != 0)
	  NewArray.Tree(getter)
	else
	  NewArray.Tree(valdef1, getter)
      } else {
	val setter: Tree = make.DefDef(
	  tree.pos, mods1, setterName(name),
	  Tree.AbsTypeDef_EMPTY_ARRAY,
	  NewArray.ValDefArray(
	    NewArray.ValDef(
	      make.ValDef(
		tree.pos, SYNTHETIC | PARAM, parameterName(0),
		tpe.duplicate(), Tree.Empty))),
	  gen.mkType(tree.pos, global.definitions.UNIT_TYPE()),
	  if ((mods1 & DEFERRED) != 0) Tree.Empty
	  else make.Assign(
	    tree.pos,
	    make.Ident(tree.pos, valname),
	    make.Ident(tree.pos, parameterName(0))));
	if ((mods1 & DEFERRED) != 0)
	  NewArray.Tree(getter, setter)
	else
	  NewArray.Tree(valdef1, getter, setter)
      }
  }

  /** Expand partial function applications of type `type'.
  *
  *  p.f(es_1)...(es_n)
  *     ==>  {
  *            private synthetic val eta$f    = p.f   // if p is not stable
  *            ...
  *            private synthetic val eta$e_i = e_i    // if e_i is not stable
  *             ...
  *
  *            (ps_1 => ... => ps_m => eta$f([es_1])...([es_m])(ps_1)...(ps_m))
  *          }
  *  tree is already attributed
  */
  def etaExpand(tree: Tree, tpe: Type): Tree = {
    val defs: TreeList = new TreeList();
    val lambda: Tree =
      toFunction(toApply(liftoutPrefix(tree, defs), tpe), tpe);
    val result: Tree = make.Block(tree.pos, defs.toArray(), lambda);
    print(tree, "eta", result);//debug
    result
  }

  private val preName = "eta$";

  /** Append to `defs' value definitions for all non-stable subexpressions
  *  of the function application `tree'
  */
  def liftoutPrefix(tree: Tree, defs: TreeList): Tree = {

    def liftoutList(trees: Array[Tree], defs: TreeList): Array[Tree] = {
      var trees1 = trees;
      for (val i <- Iterator.range(0, trees.length)) {
	val tree: Tree = trees(i);
	val tree1: Tree = liftout(tree, defs);
	if (tree1 != tree && trees1 == trees) {
	  trees1 = new Array[Tree](trees.length);
	  System.arraycopy(trees, 0, trees1, 0, trees.length);
	}
	trees1(i) = tree1;
      }
      trees1;
    }

    def liftout(tree: Tree, defs: TreeList): Tree =
      if (TreeInfo.isPureExpr(tree)) tree
      else {
	val vname: Name = Name.fromString(preName + defs.length());
	defs.append(
	  make.ValDef(
	    tree.pos, SYNTHETIC, vname, Tree.Empty, tree));
	make.Ident(tree.pos, vname)
      }

    tree match {
      case Tree$Ident(_) =>
	tree;
      case Tree$Select(qual, _) =>
	copy.Select(tree, liftout(qual, defs)).setType(null);
      case Tree$TypeApply(fn, args) =>
	copy.TypeApply(tree, liftoutPrefix(fn, defs), args).setType(null);
      case Tree$Apply(fn, args) =>
	copy.Apply(tree, liftoutPrefix(fn, defs), liftoutList(args, defs)).setType(null);
    }
  }

  /**  in patterns x  => x @ _
  *   precondition:  name != '_'
  *   post: returns *unattributed* Bind tree
  */

  def IdentPattern(tree: Tree): Tree = tree match {
    case Tree$Ident(name) =>
      if (name == Names.PATTERN_WILDCARD)
        throw new ApplicationError("nothing to desugarize");
      make.Bind(
	tree.pos,
	name,
	gen.Ident(tree.pos, global.definitions.PATTERN_WILDCARD))
	  .setType(tree.getType());
  }

  /**  in patterns x:T  => x @ _ : T
  *   pre: t is a typed variable.
  *   post: returns *unattributed* Bind tree
  */
  def TypedPattern(t: Tree$Typed): Tree = t match {
    case Tree$Typed(Tree$Ident(name), tpe) =>
      make.Bind(
	t.pos,
        name,
        make.Typed(
	  t.pos,
          gen.Ident(t.pos, global.definitions.PATTERN_WILDCARD),
          tpe));
  }

  /** f, (syms_1)...(syms_n)T    ==>    f(ps_1)...(ps_n)
  */
  def toApply(tree: Tree, tpe: Type): Tree = tpe match {
    case Type$MethodType(vparams, restpe) =>
      val res: Tree = make.Apply(tree.pos, tree, toIdents(vparams));
      toApply(res, restpe);
    case _ =>
      tree
  }

  /** e, (syms_1)...(syms_n)T    ==>    (ps_1 => ... => ps_n => e)
    */
  def toFunction(tree: Tree, tpe: Type): Tree = tpe match {
    case Type$MethodType(vparams, restpe) =>
      make.Function(tree.pos, toVparams(vparams), toFunction(tree, restpe));
    case _ =>
      tree
  }

  /** Extract value parameters from type.
  */
  def toVparams(symbols: Array[Symbol]): Array[Tree$ValDef] = {
    val vpars = new Array[Tree$ValDef](symbols.length);
    for (val i <- Iterator.range(0, symbols.length)) {
      vpars(i) = make.ValDef(
	symbols(i).pos, PARAM | SYNTHETIC, symbols(i).name,
	gen.mkType(symbols(i).pos, symbols(i).getType()),
	Tree.Empty);
    }
    vpars
  }

  /** Extract value identifiers from method type.
  *  It is assumed that all symbols are term symbols  ==>  make.Ident().
  */
  def toIdents(symbols: Array[Symbol]): Array[Tree] = {
    val idents = new Array[Tree$Ident](symbols.length);
    for (val i <- Iterator.range(0, symbols.length)) {
      idents(i) = make.Ident(symbols(i).pos, symbols(i).name);
    }
    idents.asInstanceOf[Array[Tree]];
  }

  /** Build value element definition name for case parameter.
  */
  def addCaseElement(ts: TreeList, vparam: Tree$ValDef): unit = {
    //vparam.symbol().initialize();
    ts.append(
      make.ValDef(
	vparam.pos, CASEACCESSOR, vparam.name, Tree.Empty,
	make.Ident(vparam.pos, vparam.name)
	.setSymbol(vparam.symbol())));
  }

  /** add case constructor, defintiions value and access functions.
  */
  def addCaseElements(body: Array[Tree], vparams: Array[Tree$ValDef]): Array[Tree] = {
    val stats: TreeList = new TreeList();
    for (val i <- Iterator.range(0, vparams.length)) {
      addCaseElement(stats, vparams(i));
    }
    stats.append(body);
    stats.toArray();
  }

  //debug
  def print(tree: Tree, conv: String, result: Tree): unit = {
    if (global.log()) {
      new TextTreePrinter()
      .print(tree).println()
      .print(" --" + conv + "--> ").println()
      .print(result).println().end();
    }
  }
}
}
