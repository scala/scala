/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author buraq
 */
// $Id$
package scala.tools.nsc.matching;

/** Translation of pattern matching
 */
abstract class TransMatcher {

  val global: Global;

  import global._;
  import definitions._;
  import posAssigner.atPos;

  class TransMatchPhase(prev: Phase) extends StdPhase(prev) {
    def name = "transmatcher";
    val global: TransMatcher.this.global.type = TransMatcher.this.global;
    def apply(unit: CompilationUnit): unit =
      unit.body = newTransMatcher.transform(unit.body);
  }

  def newTransMatcher = new TransMatch();

  class TransMatch extends Transformer {

    def isRegular(pat:Tree): Boolean = pat match {
      case Alternative(_)          =>  true
      case Star(_)                 =>  true;

      case Ident(_)                =>  false

      case Bind( n, pat1 )         =>  isRegular( pat1 )
      case Sequence( trees )       =>
        ( trees.length == 0 ) || isRegular( trees );

      case Apply( fn, trees )      =>
        isRegular( trees ) &&
      !((trees.length == 1) && TreeInfo.isEmptySequence( trees( 0 )))

      case Literal(_)              => false;
      case Select(_,_)             => false;
      case Typed(_,_)              => false;
      case _ => error("in TransMatch.isRegular phase: unknown node"+pat.getClass());
    }


    /** a casedef with sequence subpatterns like
     *
     *  case ..x @ ().. => body
     *
     * should be replaced straight away with
     *
     *  case    .. () .. => val x = Nil; body
     */
    def removeNilVariables( cd: CaseDef ): CaseDef = {
      var nilVars:List[Symbol] = Nil;
      def remove(pat: Tree): Tree = pat.match {
        case Alternative( _ )          => pat /* no bind/var allowed! */
        case Star( _ )                 => pat /* no bind/var allowed! */
        case Bind( id, empt @ Sequence()) =>
          nilVars = id.symbol() :: nilVars;
          empt
        case Bind( id, pat )           =>
          copy.Bind( pat, id, remove(pat) );

	case Sequence( trees ) =>
          copy.Sequence( pat, trees map remove )

        case Apply( fn,  args ) =>
          copy.Apply( pat, fn, args map remove )

	case Ident(_)          => pat
        case Literal(_)        => pat
        case Select(_,_)       => pat
        case Typed(_,_)        => pat

        case _ => error("unknown node"+pat.getClass());
      }

      cd.match {
        case CaseDef(pat, guard, body) =>
          val npat = remove(pat);
          val nbody = {
            if(nilVars.isEmpty)
              body
            else
              atPos(body.pos)(
                Block(nilVars map { x => ValDef(s, gen.mkNil) }, body)
              )
          }
        copy.CaseDef(cd, npat, guard, nbody)
      }
    }

    def handle(sel:Tree, cases:List[CaseDef]): Tree = {

      // 1. is there a regular pattern?

      val containsReg = cases.exists { isRegular };

      // @todo: remove unused variables

      if(containsReg) {
        // 2. replace nilVariables
        //@todo: bring over AlgebraicMatcher
        //val ncases  = cases.map { removeNilVariables };
        //val matcher = new PartialMatcher( currentOwner, root, restpe );
        //new AlgebraicMatcher().construct( matcher, ncases );

        //matcher.tree
        null
      } else {
        val pm = new matching.PatternMatcher() {
          val global = TransMatcher.this.global;
        }
        pm.initialize(root, currentOwner, restpe, true );
        pm.construct( cases );
        if (global.log()) {
          global.log("internal pattern matching structure");
          pm.print();
        }
        pm.toTree();
      }
    }

    override def transform(tree: Tree) = tree match {
      case Match(selector, cases) =>
        handle(transform(selector), transform1(cases));
      case _ =>
        super.transform(tree);
    }
  }
}
