/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author buraq
 */
// $Id$
package scala.tools.nsc.matching;

/** Translation of pattern matching
 */
abstract class TransMatcher extends transform.Transform
with PatternNodes
with CodeFactory
with PatternMatchers
with SequenceMatchers
with AlgebraicMatchers
with MatcherLabels
with BerrySethis
with DetWordAutoms
with NondetWordAutoms
with Autom2
with WordAutoms
with LeftTracers
with RightTracers {

  import global._;
  import definitions._;
  import posAssigner.atPos;

  val phaseName = "transmatcher";

  protected def newTransformer(unit: global.CompilationUnit): global.Transformer = {
    cunit = unit;
    new TransMatch
  }

  /** container. classes AlgebraicMatcher and SequenceMatcher get input and
   *  store their results in here. resembles the 'Memento' design pattern,
   *  could also be named 'Liaison'
   */
  abstract class PartialMatcher {

    /** owner of the code we create (input)
     */
    val owner: Symbol;

    /** the selector value (input)
     */
    val selector:Tree;

    /** tree representing the matcher (output)
     */
    var tree: Tree  = _ ;

    def pos: int = selector.pos;

    //assert( owner != null ) : "owner is null";
    //assert owner != Symbol.NONE ;
    //this.owner      = owner;

    //assert root != null;
    //assert root.type != null;
    //this.selector   = root;

    //assert this.resultType != Type.NoType;
    //this.resultType = resultType;

    //this.pos        = root.pos; // for convenience only

  }

  var cunit: CompilationUnit;

  def fresh = cunit.fresh ;

  def containsBinding(pat: Tree): Boolean = {
    var generatedVars = false;

    def handleVariableSymbol(sym: Symbol): Unit  =
      if (sym.name.toString().indexOf("$") == -1) {
        generatedVars = true; // .add(sym);
      }

    def isVariableName(name: Name): Boolean =
      ( treeInfo.isVariableName(name) ) && ( name != nme.USCOREkw ) ;

    def isVariableSymbol(sym: Symbol): Boolean =
      ( sym != null )&&( !sym.isPrimaryConstructor );

    def traverse(tree: Tree): Unit = {

      tree match {
        case x @ Ident(name) =>
          if(x.symbol != definitions.PatternWildcard)
            error("shouldn't happen?!");

        case Bind(name, subtree) =>
          var sym: Symbol = _;

        if (isVariableName(name)
            && isVariableSymbol( {sym = tree.symbol; tree.symbol} ))
          handleVariableSymbol(sym);

        traverse( subtree );

        // congruence

        case Apply(fun, args) => args foreach traverse;
        case Sequence(trees)  => trees foreach traverse
        case Star(arg)        => traverse(arg)
        case Typed(expr, tpe) => traverse(expr); // needed??

        case  _ : Select |
               _ : Alternative |
               _ : Select |
               _ : Literal =>  ; // no variables

        case _ =>
          error("unknown pattern node:" + tree + " = " + tree.getClass());
      }
    }
    traverse(pat);
    generatedVars;
  }

  class TransMatch extends Transformer {

    def isRegular(pat:Tree): Boolean = pat match {
      case Alternative(_)          =>  true
      case Star(_)                 =>  true;

      case Ident(_)                =>  false

      case Bind( n, pat1 )         =>  isRegular( pat1 )
      case Sequence( trees )       =>
        ( trees.length == 0 ) || (trees exists { isRegular });

      case Apply( fn, List(Sequence(List())))      =>
        false;

      case Apply( fn, trees )      =>
        ( trees exists { isRegular })
//      && !((trees.length == 1) && TreeInfo.isEmptySequence( trees( 0 )))

      case Literal(_)              => false;
      case Select(_,_)             => false;
      case Typed(_,_)              => false;
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
        case Bind( id, empt @ Sequence(List())) =>
          nilVars = pat.symbol /*id.symbol()*/ :: nilVars;
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

        case _ => scala.Predef.error("unknown node"+pat.getClass());
      }

      cd.match {
        case CaseDef(pat, guard, body) =>
          val npat = remove(pat);
          val nbody = {
            if(nilVars.isEmpty)
              body
            else
              atPos(body.pos)(
                Block(nilVars map {
                  x => ValDef(x, Ident(definitions.NilModule))
                }, body)
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
        val ncases  = cases.map { removeNilVariables };
        val matcher = new PartialMatcher {
          val global: TransMatcher.this.global.type = TransMatcher.this.global;
          val owner = currentOwner;
          val selector = sel ;
        }
        //new AlgebraicMatcher() {
        //  val tm: TransMatcher.this.type = TransMatcher.this;
        //}.construct( matcher, ncases );
        //matcher.tree
        null
      } else {
        val pm = new PatternMatcher();
        pm.initialize(sel, currentOwner, true );
        pm.construct( cases );
        //if (global.log()) {
        //  global.log("internal pattern matching structure");
        //  pm.print();
        //}
        pm.toTree();
      }
    }

    override def transform(tree: Tree): Tree = tree match {
      case Match(selector, cases) =>
        val ts = cases map { transform };
        handle(transform(selector), ts.asInstanceOf[List[CaseDef]]);
      case _ =>
        super.transform(tree);
    }
  }
}
