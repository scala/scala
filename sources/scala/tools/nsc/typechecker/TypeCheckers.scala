/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$
package scala.tools.nsc.typechecker;

import scala.tools.util.Position;

/** Methods to create symbols and to enter them into scopes. */
trait TypeCheckers: Analyzer {
  import symtab.Flags._;
  import global._;

  class TypeCheckPhase(prev: Phase) extends StdPhase(prev) {
    def name = "typechecker";
    val global: TypeCheckers.this.global.type = TypeCheckers.this.global;
    def apply(unit: CompilationUnit): unit =
      unit.body = new TypeChecker(startContext.make(unit)).transformTrees(unit.body);
  }

  class TypeChecker(context: Context) extends Transformer {

    import context.unit;

    val infer = new Inferencer(context);
    import infer._;

    def reportTypeError(pos: int, ex: TypeError): unit = {
      if (settings.debug.value) ex.printStackTrace();
      ex match {
	case CyclicReference(sym, info: TypeCompleter) =>
	  info.tree match {
	    case ValDef(_, _, EmptyTypeTree(), _) =>
	      error(pos, "recursive " + sym + " needs type")
	    case DefDef(_, _, _, _, EmptyTypeTree(), _) =>
	      error(pos, "recursive " + sym + " needs result type")
	    case _ =>
	      error(pos, ex.getMessage())
	  }
	case _ =>
	  error(pos, ex.getMessage())
      }
    }

    /** Mode constants
    */
    val NOmode        = 0x000;
    val EXPRmode      = 0x001;  // these 3 modes are mutually exclusive.
    val PATTERNmode   = 0x002;
    val TYPEmode      = 0x004;

    val FUNmode       = 0x10;   // orthogonal to above. When set
                                // we are looking for a method or constructor

    val POLYmode      = 0x020;  // orthogonal to above. When set
                                // expression types can be polymorphic.

    val QUALmode      = 0x040;  // orthogonal to above. When set
                                // expressions may be packages and
                                // Java statics modules.

    val SUPERmode     = 0x080;  // Goes with CONSTRmode. When set
                                // we are checking a superclass
                                // constructor invocation.

    val baseModes: int  = EXPRmode | PATTERNmode;

    val SEQUENCEmode  = 0x1000;  // only for PATTERNmode, otherwise orthogonal to above. When set
                                 // we turn "x" into "x@_"
                                 // and allow args to be of type Seq( a) instead of a

    def reenterValueParams(tree: Tree): unit = tree match {
      case DefDef(_, _, _, vparamss, _, _) =>
	for (val vparams <- vparamss; val vparam <- vparams) context.scope enter vparam.symbol
    }

    def checkStable(tree: Tree): Tree = tree;
    def checkNoEscape(pos: int, tpe: Type): Type = tpe;

    def transformConstr(tree: Tree): Tree = tree;
    def transformExpr(tree: Tree): Tree = tree;
    def transformType(tree: Tree): Tree = tree;
    def transformSuperType(tree: Tree): Tree = tree;

    def transform(tree: Tree, mode: int, pt: Type): Tree = {

      /** Turn tree type into stable type if possible and required by context. */
      def stabilize(tree: Tree, pre: Type): Tree = tree.tpe match {
        case ConstantType(base, value) =>
          Literal(value) setPos tree.pos setType tree.tpe
        case PolyType(List(), restp @ ConstantType(base, value)) =>
          Literal(value) setPos tree.pos setType restp
        case _ =>
          if (tree.symbol.hasFlag(OVERLOADED) && (mode & FUNmode) == 0)
            inferExprAlternative(tree, pt);
          if (tree.symbol.isStable && pre.isStable &&
              (pt.isStable  || (mode & QUALmode) != 0 || tree.symbol.isModule))
            tree.tpe = singleType(pre, tree.symbol);
          tree
      }

      /** Attribute an identifier consisting of a simple name or an outer reference.
       *  @param tree      The tree representing the identifier.
       *  @param name      The name of the identifier.
       *  Transformations: (1) Prefix class members with this.
       *                   (2) Change imported symbols to selections
       */
      def transformIdent(name: Name): Tree = {
	def ambiguousError(msg: String) =
	  unit.error(tree.pos, "reference to " + name + " is ambiguous;\n" + msg);
	def importTree(imp: Symbol) =
	  imp.info.asInstanceOf[ImportType].tree;
	def importedSymbol(imp: Symbol) =
	  treeInfo.importedSymbol(importTree(imp), name);
	def isExplicitImport(imp: Symbol) =
	  treeInfo.isExplicitImport(importTree(imp), name);
	//System.out.println("transforming " + name);//DEBUG

	// Compute values of the following 4 variables,
	// except that `pre' is only set for directly inherited symbols.
	// Also detect double imports.
	var sym: Symbol = NoSymbol;  // the directly found symbol
	var sym1: Symbol = NoSymbol; // the imported symbol
	var pre: Type = null;             // if symbols are class members, their prefix type
	var impSym = NoSymbol;            // if symbols are imported, the import symbol from
					  // which they were imported.
	var cx: Context = context;
	while (sym == NoSymbol && cx != NoContext) {
	  val symEntry = cx.scope.lookupEntry(name);
	  var impEntry = cx.scope.lookupEntry(nme.IMPORT);
	  while (impEntry != null) {
	    var impSym1 = impEntry.sym;
	    impEntry = cx.scope.lookupNextEntry(impEntry);
	    sym1 = importedSymbol(impSym1);
	    if (sym1 != NoSymbol) { // check whether shadowed by sym, reset to NoSymbol if yes.
	      if (symEntry != null) {
		var impEntry1 = symEntry.owner.lookupEntry(nme.IMPORT);
		while (impEntry1 != null && sym1 != NoSymbol) {
		  if (impEntry1.sym == impSym1) sym1 = NoSymbol;
		  // directly found takes precedence
		  impEntry1 = symEntry.owner.lookupNextEntry(impEntry1);
		}
	      }
	    }
	    if (sym1 != NoSymbol) { // check for ambiguous imports
	      var impSym2: Symbol = NoSymbol;   // alternative import symbol
	      var sym2: Symbol = NoSymbol;      // alternative imported symbol
	      def ambiguousImportError = ambiguousError(
		"it is imported twice in the same scope by\n" +
		importTree(impSym1) +  "\nand " + importTree(impSym2));
	      while (impEntry != null) {
		impSym2 = impEntry.sym;
		impEntry = cx.scope.lookupNextEntry(impEntry);
		if (impSym2.owner == impSym1.owner) {
		  sym2 = importedSymbol(impSym2);
		  if (sym2 != NoSymbol) {
		    if (isExplicitImport(impSym2)) {
		      if (isExplicitImport(impSym1)) ambiguousImportError;
		      sym1 = sym2;
		      impSym1 = impSym2;
		    }
		    if (isExplicitImport(impSym1)) sym2 = NoSymbol;
		  }
		}
	      }
	      if (sym2 != NoSymbol) ambiguousImportError
	    }
	  }
	  if (symEntry != null) sym = symEntry.sym;
	  cx = cx.enclClass;
	  if (cx != NoContext) {
	    pre = cx.owner.thisType;
	    if (sym == NoSymbol) sym = pre.member(name);
	    cx = cx.outer;
	  }
	}

	// detect ambiguous definition/import,
	// update `sym' to be the final resolved symbol,
	// update `pre' to be `sym's prefix type in case it is a class member,
	// and compute value of:
	var qual: Tree = EmptyTree;   // the qualififier tree if transformed tree is a select
	if (sym != NoSymbol) {
	  if (sym1 != NoSymbol)
	    ambiguousError(
	      "it is both defined in " + sym.owner +
	      " and imported subsequently by \n" + importTree(impSym));
	  else if (sym.owner.isClass && !sym.owner.isPackageClass)
	    qual = gen.This(tree.pos, pre.symbol);
	} else {
	  if (sym1 != NoSymbol) {
	    sym = sym1;
	    qual = importTree(impSym).expr.duplicate;
	    pre = qual.tpe;
	  } else {
	    error(tree.pos, "not found: " + decode(name));
	    sym = context.owner.newErrorSymbol(name);
	  }
	}
	stabilize(checkAccessible(tree, sym, pre, qual), pre);
      }

      def applyPoly(tree: Tree, tparams: List[Symbol], restpe: Type): Tree = {
        val tparams1 = tparams map (.cloneSymbol);
        val targs = context.undetparams map (tparam => EmptyTypeTree().setType(tparam.tpe));
        context.undetparams = context.undetparams ::: tparams1;
        (if (qual0.isType) AppliedTypeTree(tree, targs) else TypeApply(tree, targs))
          .setType(restpe.subst(tparams, tparams1))
      }

      /** Attribute a selection where `tree' is `qual.name'.
       *  `qual' is already attributed.
       */
      def transformSelect(tree: Tree, qual0: Tree, name: Name): Tree = {
	val qual = qual0.tpe match {
	  case PolyType(tparams, restpe) =>
	    assert(context.undetparams.isEmpty);
            applyPoly(qual, tparams, restpe);
	  case _ =>
	    qual0
	}
	val sym = qual.tpe.member(name);
	// if (sym == NoSymbol) try to insert view.
        val pre = qual match {
          case Super(_, _) => context.enclClass.owner.thisType
          case _ => qual.tpe
        }
	val tree1 = tree match {
	  case Select(_, name) => copy.Select(tree, qual, name);
	  case SelectFromTypeTree(_, name) => copy.SelectFromTypeTree(tree, qual, name)
	}
	stabilize(checkAccessible(tree1, sym, pre, qual), pre);
      }

      /** Attribute an argument list.
       *  @param pos      Position for error reporting
       *  @param meth     The symbol of the called method, or `null' if none exists.
       *  @param tparams  The type parameters that need to be instantiated
       *  @param methtype The method's type w/o type parameters
       *  @param argMode  The argument mode (either EXPRmode or PATTERNmode)
       *  @param args     The actual arguments
       *  @param pt       The proto-resulttype.
       *  @return         The vector of instantiated argument types, or null if error.
       */
      def transformArgs(pos: int, meth: Symbol, tparams: Array[Symbol], methtype: Type, argMode: int, args: Array[Tree], pt: Type): Array[Tree] = {
	val argtypes = new Array[Type](args.length);
	methtype match {
	  case Type$MethodType(params, restp) =>
	    val formals = infer.formalTypes(params, args.length);
	    if (formals.length != args.length) {
	      error(pos, "wrong number of arguments for " +
		  (if (meth == null) "<function>" else meth) +
		  ArrayApply.toString(formals.asInstanceOf[Array[Object]], "(", ",", ")"));
	      return null;
	    }
	    if (tparams.isEmpty) {
	      List.map2(args, formals) ((arg, formal) => transform(arg, argMode, formal))
	    } else {
	      val targs = protoTypeArgs(tparams, params, restp, pt);
	      val argpts = formals map (.subst(tparams, targs));
	      val args1 = List.map2(args, argpts)((arg, argpt) =>
		transform(arg, argMode | POLYmode, argpt));
	      // targs1: same as targs except that every WildcardType is mapped to
	      // formal parameter type.
	      val targs1 = List.map2(targs, tparams)((targ, tparam) =>
		if (targ == WildcardType) tparam.tpe else targ);


	  { var i = 0; while (i < args.length) {
	    argtypes(i) = args(i).getType().deconst();
	    argtypes(i) match {
	      case Type$PolyType(tparams1, restype1) =>
		argtypes(i) = infer.argumentTypeInstance(
		  tparams1, restype1,
		  formals(i).subst(tparams, targs1),
		  argpts(i));
	      case _ =>
	    }
	    i = i + 1
	  }}
	}
	//   desugarizing ident patterns
	if (params.length > 0 && (params(params.length-1).flags & REPEATED) != 0) {
	  if ((mode & PATTERNmode) != 0) {
	    def desug_allIdentPatterns(trees: Array[Tree], currentOwner: Symbol): unit = {
	      var i = 0; while (i < trees.length) {
		trees(i) match {
		  case Tree.Ident(name) =>
		    if (name != Names.PATTERN_WILDCARD) {
		      val vble: Symbol = context.scope.lookup(name);
		      trees(i) = desugarize.IdentPattern(trees(i)).setSymbol(vble)
			.setType(vble.getType());
		    } else {
                      trees(i) = gen.Ident(trees(i).pos, definitions.PATTERN_WILDCARD);
                    }
		  case _ =>
		}
		i = i + 1
	      }
	    }
    	    desug_allIdentPatterns(args, context.owner);
	  } else {
	    assert(args.length != params.length ||
		   !(args(params.length-1).isInstanceOf[Tree.Sequence]));
	  }
	}
	argtypes;

      case Type$PolyType(tparams1, restp) =>
	var tparams2: Array[Symbol] = tparams1;
	if (tparams.length != 0) {
	  tparams2 = new Array[Symbol](tparams.length + tparams1.length);
	  System.arraycopy(tparams, 0, tparams2, 0, tparams.length);
	  System.arraycopy(tparams1, 0, tparams2, tparams.length, tparams1.length);
	}
	transformArgs(pos, meth, tparams2,
		      infer.skipViewParams(tparams2, restp), argMode, args, pt)

      case Type.ErrorType =>
	var i = 0; while (i < args.length) {
	  args(i) = transform(args(i), argMode, Type.ErrorType);
	  argtypes(i) = args(i).getType().deconst();
	  i = i + 1
	}
	argtypes

      case Type.OverloadedType(alts, alttypes) if (alts.length == 1) =>
        transformArgs(pos, alts(0), tparams, alttypes(0), argMode, args, pt)

      case _ =>
	var i = 0; while (i < args.length) {
	  args(i) = transform(args(i), argMode, Type.AnyType);
	  argtypes(i) = args(i).getType().deconst();
	  i = i + 1
	}
	argtypes
    }
  }



/*      tree
    }
  }
}
*/

      def transformApply(tree: Tree, fn: Tree, args: List[Tree]): Tree = {
        var fn1 =
          if ((mode & PATTERNmode) != 0) transform(fn, mode | FUNmode & ~SEQUENCEmode, pt)
          else transform(fn, mode | FUNmode, WildcardType);

	// if function is overloaded filter all alternatives that match
	// number of arguments and expected result type.
	if (fn1.hasSymbol && fn1.symbol.hasFlag(OVERLOADED)) {
	  val sym1 = fn1.symbol filter (alt =>
	    isApplicable(fn1.symbol.info.prefix.memberType(alt), argtypes, pt));
	  fn1.setSymbol(sym1).setType(fn1.symbol.info.prefix.memberType(sym1));
	}

	// handle the case of application of match to a visitor specially
	args match {
	  case List(Visitor(_)) if (treeInfo.methSymbol(fn1) == Any_match) =>
	    return transformMatch(fn1, args.head);
	  case _ =>
	}
      }

      def transformApply(fn: Tree, args: List[Tree]): Tree = {
	fn.tpe match {
	  case OverloadedType(pre, alts) =>
            val args1 = args map (arg => transform(arg, argMode, WildcardType));
	    inferMethodAlternative(fn, args1 map (.tpe.deconst), pt);
            transformApply(fn, args1);
          case PolyType(tparams, restpe) =>
            transformApply(applyPoly(fn, tparams, restpe));
          case MethodType(formals0, restpe) =>
	    val formals = formalTypes(formals, args.length);
	    if (formals.length != args.length) {
	      error(pos, "wrong number of arguments for " + treeSymTypeMsg(fn1));
              setError(tree)
            }
            val tparams = context.undetparams;
	    if (tparams.isEmpty) {
	      val args1 = List.map2(args, formals) ((arg, formal) =>
                transform(arg, argMode, formal));

	    } else {
	      val targs = protoTypeArgs(tparams, formals, restp, pt);
	      val argpts = formals map (.subst(tparams, targs));
	      val args1 = List.map2(args, argpts1)((arg, argpt) =>
		transform(arg, argMode | POLYmode, argpt));
	      // targs1: same as targs except that every WildcardType is mapped to
	      // formal parameter type.
	      val targs1 = List.map2(targs, tparams)((targ, tparam) =>
		if (targ == WildcardType) tparam.tpe else targ);
              val argpts1 = formals map (.subst(tparams, targs1));
              val argtpes = List.map3(args, argpts, argpts1)(inferArgumentTypeInstance);
              inferMethodInstance(fn, argtpes, pt);
              transformApply(fn, args1)
            }



            val tparams1 = tparams map (.cloneSymbol);
            val restpe1 = restpe.substSym(tparams, tparams1);


            copy.Apply(tree, fn1, args1).setType(???)
          case MethodType(formals, restpe) =>
            transformMonoApply(fn1, formals, restpe, args)



	// type arguments with formals as prototypes if they exist.
	val argtypes = transformArgs(tree.pos, fn1.symbol, fn1.tpe,
				     mode & (PATTERNmode | EXPRmode), args, pt);
	if (argtypes == null || argtypes exists (.isError)) return setError(tree);

        var args1 = null;
	// resolve overloading



	      pt);
	  case _ =>
	}

	// infer method instance


	  val pattp: Type = matchQualType(fn1);
	  if (pattp.isError()) {
		return setError(tree)
	      } else if (pattp != Type.NoType) {
		if (infer.isFullyDefined(pattp) &&
		    !(fn1.getType().isInstanceOf[Type$PolyType] &&
		      pattp.containsSome(fn1.getType().typeParams()))) {
		  val fn2: Tree = desugarize.postMatch(fn1, context.enclClass.owner);
		  val arg1: Tree = transformVisitor(args(0), pattp, pt);
		  return copy.Apply(tree, fn2, NewArray.Tree(arg1))
			.setType(arg1.getType());
		} else {
                  error(tree.pos, "expected pattern type of cases could not be determined");
                  return errorTermTree(tree)
		}
	      }
	    }



*/
