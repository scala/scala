/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author
 */
// $Id$
package scala.tools.nsc.transform;

/*<export>*/
/** - uncurry all symbol and tree types (@see UnCurryPhase)
 *  - for every curried parameter list:  (ps_1) ... (ps_n) ==> (ps_1, ..., ps_n)
 *  - for every curried application: f(args_1)...(args_n) ==> f(args_1, ..., args_n)
 *  - for every type application: f[Ts] ==> f[Ts]() unless followed by parameters
 *  - for every use of a parameterless function: f ==> f()  and  q.f ==> q.f()
 *  - for every def-parameter:  x: => T ==> x: () => T
 *  - for every use of a def-parameter: x ==> x.apply()
 *  - for every argument to a def parameter `x: => T':
 *      if argument is not a reference to a def parameter:
 *        convert argument `e' to (expansion of) `() => e'
 *  - for every repated parameter `x: T*' --> x: Seq[a].
 *  - for every argument list that corresponds to a repeated parameter
 *       (a_1, ..., a_n) => (Seq(a_1, ..., a_n))
 *  - for every argument list that is an escaped sequence
 *       (a_1:_*) => (a_1)g
 *  - convert implicit method types to method types
 *  - convert non-trivial catches in try statements to matches
 */
/*</export>*/
abstract class UnCurry extends InfoTransform {
  import global._;                  // the global environment
  import definitions._;             // standard classes and methods
  import typer.{typed};             // methods to type trees
  import posAssigner.atPos;         // for filling in tree positions

  val phaseName: String = "uncurry";
  def newTransformer(unit: CompilationUnit): Transformer = new UnCurryTransformer(unit);

  private val uncurry = new TypeMap {
    def apply(tp: Type): Type = tp match {
      case MethodType(formals, MethodType(formals1, restpe)) =>
	apply(MethodType(formals ::: formals1, restpe))
      case mt: ImplicitMethodType =>
	apply(MethodType(mt.paramTypes, mt.resultType))
      case PolyType(List(), restpe) =>
	apply(MethodType(List(), restpe))
      case PolyType(tparams, restpe) =>
	PolyType(tparams, apply(MethodType(List(), restpe)))
      case TypeRef(pre, sym, List(arg)) if (sym == ByNameParamClass) =>
	apply(functionType(List(), arg))
      case TypeRef(pre, sym, args) if (sym == RepeatedParamClass) =>
	apply(rawTypeRef(pre, SeqClass, args));
      case _ =>
	mapOver(tp)
    }
  }

  /** - return symbol's transformed type,
   *  - if symbol is a def parameter with transformed type T, return () => T
   */
  def transformInfo(sym: Symbol, tp: Type): Type = if (sym.isType) tp else uncurry(tp);

  class UnCurryTransformer(unit: CompilationUnit) extends Transformer {

    private var inPattern = false;

    override def transform(tree: Tree): Tree = postTransform(mainTransform(tree));

    /* Is tree a reference `x' to a call by name parameter that neeeds to be converted to
     * x.apply()? Note that this is not the case if `x' is used as an argument to another
     * call by name parameter.
     */
    def isByNameRef(tree: Tree): boolean =
      tree.isTerm && tree.hasSymbol &&
      tree.symbol.tpe.symbol == ByNameParamClass && tree.tpe == tree.symbol.tpe.typeArgs.head;

    /** Uncurry a type of a tree node.
     *  This function is sensitive to whether or not we are in a pattern -- when in a pattern
     *  additional parameter sections of a case class are skipped.
     */
    def uncurryTreeType(tp: Type): Type = tp match {
      case MethodType(formals, MethodType(formals1, restpe)) if (inPattern) =>
	uncurryTreeType(MethodType(formals, restpe))
      case _ =>
	uncurry(tp)
    }

    def transformArgs(pos: int, args: List[Tree], formals: List[Type]) = {
      if (formals.isEmpty) {
	assert(args.isEmpty); List()
      } else {
	val args1 =
          formals.last match {
            case TypeRef(pre, sym, List(elempt)) if (sym == RepeatedParamClass) =>
	      def mkSequence(args: List[Tree]) =
                atPos(pos)(ArrayValue(TypeTree(elempt), args) setType formals.last);
	      if (args.isEmpty) List(mkSequence(args))
	      else {
	        val suffix = args.last match {
		  case Typed(arg, Ident(name)) if name == nme.WILDCARD_STAR.toTypeName =>
		    arg setType seqType(arg.tpe)
		  case _ =>
		    mkSequence(args.drop(formals.length - 1))
	        }
	        args.take(formals.length - 1) ::: List(suffix)
	      }
            case _ => args
          }
	List.map2(formals, args1) ((formal, arg) =>
	  if (formal.symbol != ByNameParamClass) arg
	  else if (isByNameRef(arg)) arg setType functionType(List(), arg.tpe)
	  else typer.atOwner(currentOwner).typed(Function(List(), arg) setPos arg.pos))
      }
    }

    def mainTransform(tree: Tree): Tree = (tree match {
      case Apply(fn, args) =>
	val formals = fn.tpe.paramTypes;
        copy.Apply(tree, transform(fn), transformTrees(transformArgs(tree.pos, args, formals)))
      case CaseDef(pat, guard, body) =>
        inPattern = true;
	val pat1 = transform(pat);
	inPattern = false;
	copy.CaseDef(tree, pat1, transform(guard), transform(body))
      case _ =>
        val tree1 = super.transform(tree);
	if (isByNameRef(tree1))
	  typed(atPos(tree1.pos)(
	    Apply(Select(tree1 setType functionType(List(), tree1.tpe), nme.apply), List())))
	else tree1;
    }) setType uncurryTreeType(tree.tpe);

    def postTransform(tree: Tree): Tree = atPhase(phase.next) {
      def applyUnary(tree: Tree): Tree =
        if (tree.symbol.isMethod && (!tree.tpe.isInstanceOf[PolyType] || tree.tpe.typeParams.isEmpty)) {
	  if (!tree.tpe.isInstanceOf[MethodType]) tree.tpe = MethodType(List(), tree.tpe);
	  atPos(tree.pos)(Apply(tree, List()) setType tree.tpe.resultType)
	} else if (tree.isType && !tree.isInstanceOf[TypeTree]) {
	  TypeTree(tree.tpe) setPos tree.pos
	} else {
	  tree
	}
      tree match {
        case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          copy.DefDef(tree, mods, name, tparams, List(List.flatten(vparamss)), tpt, rhs);
        case Try(body, catches, finalizer) =>
          if (catches forall treeInfo.isCatchCase) tree
          else {
            val exname = unit.fresh.newName("ex$");
            val cases =
              if (catches exists treeInfo.isDefaultCase) catches
              else catches ::: List(CaseDef(Ident(nme.WILDCARD), EmptyTree, Throw(Ident(exname))));
            val catchall =
              atPos(tree.pos) {
                CaseDef(
                  Bind(exname, Ident(nme.WILDCARD)),
                  EmptyTree,
                  Match(Ident(exname), cases))
              }
            System.out.println("rewrote try: " + catches + " ==> " + catchall);
            val catches1 = typer.atOwner(currentOwner).typedCases(
              tree, List(catchall), ThrowableClass.tpe, WildcardType);
            copy.Try(tree, body, catches1, finalizer)
          }
	case Apply(Apply(fn, args), args1) =>
	  copy.Apply(tree, fn, args ::: args1)
	case Ident(name) =>
	  if (name == nme.WILDCARD_STAR.toTypeName)
	    unit.error(tree.pos, " argument does not correspond to `*'-parameter");
	  applyUnary(tree);
	case Select(_, _) =>
	  applyUnary(tree)
	case TypeApply(_, _) =>
	  applyUnary(tree)
	case _ =>
	  tree
      }
    }
  }
}
