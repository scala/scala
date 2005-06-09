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
 *       (a_1:_*) => (a_1)
 *  - convert implicit method types to method types
 *  - todo: check-no-double-def in erasure
 */
/*</export>*/
abstract class UnCurry extends InfoTransform {
  import global._;                  // the global environment
  import definitions._;             // standard classes and methods
  import typer.{typed};             // methods to type trees
  import posAssigner.atPos;         // for filling in tree positions

  protected val phaseName: String = "uncurry";
  protected def newTransformer(unit: CompilationUnit): Transformer = new UnCurryTransformer(unit);

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
	apply(TypeRef(pre, SeqClass, args));
      case _ =>
	mapOver(tp)
    }
  }

  /** - return symbol's transformed type,
   *  - if symbol is a def parameter with transformed type T, return () => T
   */
  def transformInfo(sym: Symbol, tp: Type): Type = uncurry(tp);

  class UnCurryTransformer(unit: CompilationUnit) extends Transformer {

    private var inPattern = false;

    override def transform(tree: Tree): Tree = {
      def transformArgs(args: List[Tree], formals: List[Type]) = {
	if (formals.isEmpty) {
	  assert(args.isEmpty); List()
	} else {
	  def mkSequence(args: List[Tree], pt: Type): Tree = typed { atPos(tree.pos) {
            if (inPattern) Sequence(args) setType pt
            else Apply(gen.mkRef(SeqFactory), args)
          }}
	  val args1 =
            formals.last match {
              case TypeRef(pre, sym, List(pt)) if (sym == RepeatedParamClass) =>
	        if (args.isEmpty) List(mkSequence(args, pt))
	        else {
		  val suffix = args.last match {
		    case Typed(arg, Ident(name)) if name == nme.WILDCARD_STAR.toTypeName =>
		      arg setType seqType(arg.tpe)
		    case _ =>
		      mkSequence(args.drop(formals.length - 1), pt)
		  }
		  args.take(formals.length - 1) ::: List(suffix)
	        }
              case _ => args
            }
	  List.map2(formals, args1) ((formal, arg) =>
	    if (formal.symbol == ByNameParamClass)
	      arg.tpe match {
		case TypeRef(pre, sym, List(targ)) if (sym == ByNameParamClass) =>
		  arg setType functionType(List(), targ)
		case _ =>
		  typed(Function(List(), arg) setPos arg.pos)
	      }
	    else arg)
	}
      }
      val prevtpe = tree.tpe;
      var result = tree match {
	case Apply(fn, args) =>
          copy.Apply(tree, super.transform(fn),
                     super.transformTrees(transformArgs(args, fn.tpe.paramTypes)))
        case CaseDef(pat, guard, body) =>
          inPattern = true;
          val pat1 = super.transform(pat);
          inPattern = false;
          copy.CaseDef(tree, pat1, super.transform(guard), super.transform(body))
	case _ =>
          super.transform(tree)
      } setType uncurry(tree.tpe);
      result match {
	case Apply(Apply(fn, args), args1) =>
	  result = copy.Apply(result, fn, args ::: args1)
	case Ident(name) =>
          if (name == nme.WILDCARD_STAR.toTypeName)
	    unit.error(tree.pos, " argument does not correspond to `*'-parameter");
          else if (prevtpe.symbol == ByNameParamClass)
	    result = typed(atPos(result.pos)(Select(result, nme.apply)))
	case _ =>
      }
      result.tpe match {
	case MethodType(List(), restpe) =>
	  if (!prevtpe.isInstanceOf[MethodType])
	    result = typed(atPos(result.pos)(Apply(result, List())))
      }
      result
    }
  }
}
