/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.transformer.matching;

import scalac.*;
import scalac.ast.*;
import scalac.atree.AConstant;
import scalac.symtab.*;

import PatternNode.*;
import Tree.*;

import scalac.transformer.TransMatch.Matcher ;
import scalac.util.Name ;
import scalac.util.Names ;
//import scalac.ast.printer.TextTreePrinter ;

import java.util.Vector ;
import java.util.Iterator ;

/** Matthias' algebraic pattern matcher, with some things factored out
 *  @author Matthias Zenger, Burak Emir
 */

public class AlgebraicMatcher extends PatternMatcher {

      Matcher _m;

      /** constructor
       */
      public AlgebraicMatcher( Unit unit ) {
            super( unit );
	    this.delegateSequenceMatching = true;
	    this.optimize = false;
      }

      /** constructs an algebraic pattern matcher from cases
       */
      public void construct( Matcher m, Tree[] cases) {
            construct(m, cases, true);
      }

      /** constructs an algebraic pattern matcher from cases
       */
      public void construct( Matcher m, Tree[] cases, boolean doBinding) {
            this._m = m;
	    super.initialize( _m.selector, _m.owner, _m.resultType, doBinding );
            for( int i = 0; i < cases.length; i++ ) {
                enter( cases[ i ] );//(CaseDef) cases[i], i);
            }
            if (unit.global.log()) {
                unit.global.log("internal pattern matching structure");
                print();
            }
            _m.tree = toTree();
      }


      /** initializes this AlgebraicMatcher, see Matcher.initialize
      void initialize() {
      }
       */

    boolean isStarApply( Tree.Apply tree ) {
	Symbol params[] = tree.fun.type.valueParams();
	//System.err.println( tree.fun.type.resultType().symbol() );
	return  (tree.args.length == 1)
	    && (tree.type.symbol().flags & Modifiers.CASE) != 0
	    && params.length > 0
	    && (params[params.length-1].flags & Modifiers.REPEATED) != 0;
    }

//////////// generator methods

    public Tree toTree() {
        Tree[] ts = {
            gen.ValDef(root.symbol(), _m.selector ),
            gen.ValDef(resultVar,
                       gen.mkDefaultValue(_m.pos, resultVar.info()) )};
        Tree res = gen.If( toTree(root.and),
                           gen.Ident( _m.pos, resultVar ),
                           cf.ThrowMatchError( _m.pos, _m.resultType ));
        /*
            gen.If(
                _m.pos,
                toTree(root.and),
                gen.Ident( _m.pos, resultVar ),
                cf.ThrowMatchError( _m.resultType ));
        */
        return gen.mkBlock(_m.pos, ts, res);
    }

    protected Tree toTree(PatternNode node, Tree selector) {
	//System.err.println("AM.toTree called"+node);
        if (node == null)
            return gen.mkBooleanLit(_m.pos, false);
        switch (node) {
        case SeqContainerPat( _, _ ):
	    return  callSequenceMatcher( node,
					 selector );
        default:
	    return super.toTree( node, selector );
        }
    }

      /** collects all sequence patterns and returns the default
       */
      PatternNode collectSeqPats( PatternNode node,
                                  Vector seqPatNodes,
                                  Vector bodies ) {

              PatternNode defaultNode = null;

              do {
                    if( node == null )
                          break;// defaultNode = node;
                    else
                          switch( node ) {
                    case SeqContainerPat( _, _ ):
                          seqPatNodes.add( node );
                          bodies.add( toTree( node.and ) );
                          node = node.or;
                          break;
                    default:
                          defaultNode = node;
                    }
              } while (defaultNode == null) ;

              return defaultNode;
      }

      Tree callSequenceMatcher( PatternNode node,
                                Tree selector) {

              /*    ???????????????????????? necessary to test whether is a Seq?
		    gen.If(selector.pos,
                             maybe cf.And( cf.Is(selector, seqpat.type())
                             ...
              */

              // translate the _.and subtree of this SeqContainerPat

	  Vector seqPatNodes = new Vector();
	  Vector bodies      = new Vector();

	  PatternNode defaultNode = collectSeqPats( node,
						    seqPatNodes,
						    bodies );

	  Tree defaultCase = toTree( defaultNode, selector );

	  SequenceMatcher wordRec = new SequenceMatcher(unit);

	  Matcher m = new Matcher( _m.owner,
				   selector,
				   defs.BOOLEAN_TYPE() );

	  Tree pats[] = new Tree[ seqPatNodes.size() ];
	  Tree body[] = new Tree[ seqPatNodes.size() ];

	  Object tmp[] = bodies.toArray();
	  int j = 0;
	  for( Iterator it = seqPatNodes.iterator();
                   it.hasNext();) {
	      pats[ j ]  = ((SeqContainerPat) it.next()).seqpat;
	      body[ j ] = (Tree) tmp[j];
	      j++;
	  }
	  //Tree defaultTree = toTree( node.or, selector ); // cdef.body ;

	  wordRec.construct( m, pats, body, defaultCase, doBinding );

	  //_m.defs.addAll( m.defs  );

	  return m.tree;
      }

}
