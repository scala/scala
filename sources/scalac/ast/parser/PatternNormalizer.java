/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002-2003, LAMP/EPFL         **
** /_____/\____/\___/\____/____/                                        **
**                                                                      **
** $Id$
\*                                                                      */

package scalac.ast.parser;

import scalac.Unit;
import scalac.ast.*;
import scalac.util.Name;
import Tree.*;
import java.util.HashMap;

import scalac.util.Names;

/** contains algorithms for `checking' and `normalizing' patterns
 *
 *  @author  Burak Emir
 *  @version 1.0
 */

public class PatternNormalizer {

    /** the compilation unit - needed for error reporting
     */
    Unit unit;

    /** the tree factory
     */
    public TreeFactory make;

    // constructor ... unit is needed to display errors

    public PatternNormalizer( Unit unit ) {
	this.unit = unit;
        this.make = unit.global.make;
    }

    //
    ///////// CHECKING patterns for well-formedness ///////////////////////////////
    //


    /** checks whether TO DO TO DO TO DO
     *  - @-recursion occurs only at the end just below a sequence
     *  returns true if the tree is ok.
     *  inSeq: flag, true if we are in a sequence '[' ... ']'
     *  t: the tree to be checked
     */
    protected boolean check1( Tree t, boolean inAlt ) {
	switch( t ) {

	case Literal( _ ):
	    return true;

	case Apply( _, Tree[] args ):
	    return check1( args, inAlt );

	case Sequence( Tree[] trees ):
	    return check1( trees, inAlt );

	case Alternative( Tree[] trees ):
	    return check1( trees, true );

	case Bind( Name var, Tree tree ):
	    if(( inAlt )
	       &&( var.toString().lastIndexOf("$") == -1)) {

		unit.error( t.pos,
			      "variable binding not allowed under alternative");
		return false;
	    }
	    this.boundVars.put( var /*t.symbol()*/, Boolean.FALSE );
	    /*
              boolean result = check( tree, inSeq );
              if(((Boolean) this.boundVars.get( t.symbol() ))
	      .booleanValue()) { // occurs recursively
	      // do something to RESTRICT recursion
              }
	    */
	    return check1( tree, inAlt );

	case Typed( _, _):
	    return true;

	case Ident( Name var ):
	    if (inAlt && var.isVariable() && var != Names.PATTERN_WILDCARD &&
		var.lastPos((byte)'$') == -1) {
		unit.error( t.pos,
			    "variable not allowed under alternative");
		return false;
	    }
              /*
              System.out.println( t.symbol().toString() );
              */
	    if(( this.boundVars.containsKey( var /*t.symbol()*/ ))
	       &&( var.toString().lastIndexOf("$") == -1)) {
		  unit.error( t.pos,
			      "recursive patterns not allowed");

		  //this.boundVars.put( t.symbol(), Boolean.TRUE ); //mark recursive
                    //System.out.println( t.symbol() + "occurs recursively");
              }

	    return true;

	case Select( _, _ ):
	    return true;

            /*
	case Empty: // may appear just as Sequence Sequence
	    if ( !inSeq )
		unit.error( t.pos,
			    "empty subsequence without surrounding '['']'");
	    return inSeq;
	    */
	default:
	    unit.error( t.pos, "whut'z dis ?"+t.toString()); // never happens
	}
	return false;

    }

    /** checkPat for every tree in array of trees, see below
     */
      protected boolean check1( Tree[] trees, boolean inAlt ) {
	for( int i = 0; i < trees.length; i++ )
	    if( !check1(  trees[ i ], inAlt ))
		return false;
	return true;
    }

      // if this map contains a symbol as a key, it is bound.
      // if this symbol is mapped to Boolean.True, it occurs recursively
    HashMap/*Name=>Boolean*/ boundVars;

      /**  method
       */
      public boolean check( Tree pat ) {
            this.boundVars = new HashMap();
            return check1( pat, false );
    }


    //
    /////////////// NORMALIZING patterns /////////////////////////////////////////////////////////////
    //

    boolean isEmptySequence( Tree tree ) {
	switch( tree ) {
	case Sequence( Tree[] trees ):
	    //return ((trees.length == 1)&&( trees[ 0 ] == Tree.Empty ));
	    return trees.length == 0;
	default:
	    return false;
	}
    }

    boolean isSequence( Tree tree ) {
	switch( tree ) {
	case Sequence( _ ):
	    return true;
	default:
	    return false;
	}
    }


    /** appends every non-empty tree in trees to ts
     */
    void appendNonEmpty( TreeList ts, Tree[] trees ) {
	for( int i = 0; i < trees.length; i++ )
	    if( !isEmptySequence( trees[ i ] ) )
		ts.append( trees[ i ] );
    }
    /** appends tree to ts if it is non-empty, leaves ts unchanged.
     */
    void appendNonEmpty( TreeList ts, Tree tree ) {
	//if( tree != Tree.Empty )
	if ( !isEmptySequence(tree))
	    ts.append( tree );
    }

    /** takes a (possibly empty) TreeList and make a Sequence out of it
     */
    Tree treeListToSequence( int pos, TreeList ts ) {
	//if( ts.length() == 0 ) // artefact of old version, where empty sequence was Subsequence node. delete soon
	//return make.Sequence( pos, Tree.EMPTY_ARRAY);
	return make.Sequence( pos, ts.toArray() );
    }



    /** (1) nested `Alternative' nodes are flattened (( tree traversal with clique contraction ))
     *       if all branches are empty, empty subsequence is returned.
     *       variables x are replaced by bindings x @ _
     */

    // apply `flattenAlternative' to each tree in ts
    public Tree[] flattenAlternatives( Tree[] ts ) {
	Tree[] res = new Tree[ ts.length ];
	for( int i = 0; i < ts.length; i++ )
	    res[ i ] = flattenAlternative( ts[ i ] );
	return res;
    }

    // main algo for (1)
    public Tree flattenAlternative( Tree tree ) {
	switch( tree ) {
	case Alternative( Tree[] choices ):
	    TreeList cs = new TreeList();
	    for( int i = 0; i < choices.length; i++ ) {
		Tree child = choices[ i ];
		switch( child ) {

		case Alternative( Tree[] child_choices ): // grab its flattened children
		    cs.append( flattenAlternativeChildren( child_choices ) );
		    break;
		default:
		    cs.append( child );
		}
	    }
	    Tree[] newtrees = cs.toArray();
	    switch( newtrees.length ) {
	    case 1:
		return newtrees[ 0 ];
	    case 0:
		return make.Sequence( tree.pos, Tree.EMPTY_ARRAY );
	    default:
		return make.Alternative( tree.pos, cs.toArray() );
	    }

	    // recursive call
	case Sequence( Tree[] trees):
	    return make.Sequence( tree.pos, flattenAlternatives( trees ));
	case Bind( Name var, Tree body ):
	    return make.Bind( tree.pos, var, flattenAlternative( body ));
	default:
	    return tree; // no alternatives can occur
	}
    }

    // main algo for (1), precondition: choices are children of an Alternative node
    public TreeList flattenAlternativeChildren( Tree[] choices ) {
	boolean allEmpty = true;
	TreeList cs = new TreeList();
	for( int j = 0; j < choices.length; j++ ) {
	    Tree tree = flattenAlternative( choices[ j ] ); // flatten child
	    switch( tree ) {
	    case Alternative( Tree[] child_choices ):
		int tmp = cs.length();
		appendNonEmpty( cs, child_choices );
		if( cs.length() != tmp )
		    allEmpty = false;
		break;
	    default:
		cs.append( tree );
		allEmpty = allEmpty && TreeInfo.isEmptySequence( tree );
	    }
	}
	if( allEmpty ) {
	    cs.clear();
	    cs.append(make.Sequence(choices[0].pos, Tree.EMPTY_ARRAY));
	}
	return cs;
    }



    /** (2) nested `Sequence' nodes are flattened (( clique elimination ))
     *      nested empty subsequences get deleted
     */

    // apply `flattenSequence' to each tree in trees
    public Tree[] flattenSequences( Tree[] trees ) {
	Tree[] res = new Tree[ trees.length ];
	for( int i = 0; i < trees.length; i++ )
	    res[ i ] = flattenSequence( trees[ i ] );
	return res;
    }
    // main algo for (2)
    public Tree flattenSequence( Tree tree ) {
	//System.out.println("flattenSequence of "+tree);
	switch( tree ) {
	    /*
	case Sequence( Tree[] trees ):
	    trees = flattenSequences( trees );
	    if(( trees.length == 1 )&&( isEmptySequence( trees[ 0 ] )))
		trees = Tree.EMPTY_ARRAY;
	    return make.Sequence( tree.pos, trees );
	    */
	case Sequence( Tree[] trees ):
	    TreeList ts = new TreeList();
	    for( int i = 0; i < trees.length; i++ ) {
		Tree child = trees[ i ];
		switch( child ) {
		case Sequence( Tree[] child_trees ): // grab its flattened children
		    ts.append( flattenSequenceChildren( child_trees ) );
		    break;
		default:
		    ts.append( child );
		}
	    }
	    /*
	      System.out.print("ts = ");
	    for(int jj = 0; jj<ts.length(); jj++) {
		System.out.print(ts.get( jj ).toString()+" ");
	    }
	    System.out.println();
	    */
	    return treeListToSequence( tree.pos, ts ) ;

	    // recursive call
	case Alternative( Tree[] choices ):
	    return make.Alternative( tree.pos, flattenSequences( choices ));
	case Bind( Name var, Tree body ):
	    return make.Bind( tree.pos, var, flattenSequence( body ));
	default:
	    return tree;
	}
    }

    // main algo for (2), precondition: trees are children of a Sequence node
    public TreeList flattenSequenceChildren( Tree[] trees ) {
	TreeList ts = new TreeList();
	for( int j = 0; j < trees.length; j++ ) {
	    Tree tree = flattenSequence( trees[ j ] );
	    switch( tree ) {
	    case Sequence( Tree[] child_trees ):
		appendNonEmpty( ts, child_trees );
		break;
	    default:
		appendNonEmpty( ts, tree );
	    }
	}
	return ts;
    }


    /** (3) in `Sequence':
     *             children of direct successor nodes labelled `Sequence' are moved up
     *      (( tree traversal, left-to-right traversal ))
     */

    /** applies `elimSequence' to each tree is ts
     */
    public Tree[] elimSequences( Tree[] ts ) {
	Tree[] res = new Tree[ ts.length ];
	for( int i = 0; i < ts.length; i++ )
	    res[ i ] = elimSequence( ts[ i ] );
	return res;
    }

    public Tree elimSequence( Tree tree ) {
	switch( tree ) {
	case Sequence( Tree[] trees ):
	    // might be empty ...
	    Tree[] newtrees = mergeHedge( trees ).toArray();
	    if(( newtrees.length == 1 )&&( isEmptySequence( newtrees[ 0 ] )))
		return make.Sequence( tree.pos, Tree.EMPTY_ARRAY );
	    return make.Sequence( tree.pos, newtrees );

	    // recurse
	    //case Sequence( Tree[] trees ): // after normalization (2), Sequence is flat
	    /*
	    TreeList ts = new TreeList();
	    for( int i = 0; i < trees.length; i++ ) {
		Tree t = trees[ i ];
		//if( t != Tree.Empty )                          // forget empty subsequences
		    ts.append( elimSequence( t )); // recurse
	    }
	    return treeListToSequence( tree.pos, ts );
	    */
	    //return make.Sequence( tree.pos, elimSequences( trees ));
	case Alternative( Tree[] choices ):
	    Tree result = make.Alternative( tree.pos, elimSequences( choices ) );
	    return flattenAlternative( result ); // apply
	case Bind( Name var, Tree body ):
	    return make.Bind( tree.pos, var, elimSequence( body ));
	default:
	    return tree; // nothing to do
	}
    }

    /** runs through an array of trees, merging adjacent `Sequence' nodes if possible
     *  returns a (possibly empty) TreeList
     *  precondition: trees are children of a Sequence node
     */
    TreeList mergeHedge( Tree[] trees ) {
	    TreeList ts = new TreeList();
	    if( trees.length > 1 ) {    // more than one child
		Tree left  = trees[ 0 ];
		Tree right = null;;
		Tree merge = null;
		for( int i = 0; i < trees.length - 1; i++) {
		    right = trees[ i+1 ];
		    merge = mergeThem( left, right );
		    if( merge != null ) {
			left = merge;
		    } else {
			ts.append( left );
			left = right;
		    }
		}
		if( merge!= null ) {
		    ts.append( merge );
		} else {
		    if( right != null )
			ts.append( right );
		}
	    } else if ( trees.length == 1 ) {
                  //if( trees[ 0 ] != Tree.Empty )
		    ts.append( trees[ 0 ] ); // append the single child
	    }
	    return ts;
    }

    /** if either left or right are subsequences, returns a new subsequences node
     *  with the children of both, where any occurences of Tree.Empty are removed
     *  otherwise, returns null. "move concatenation to the top"
     */
    Tree mergeThem( Tree left, Tree right ) {
	switch( left ) {
	case Sequence( Tree[] treesLeft ):             // left tree is subsequence
	    TreeList ts = new TreeList();
	    appendNonEmpty( ts, treesLeft );
	    switch( right ) {
	    case Sequence( Tree[] treesRight ):
		appendNonEmpty( ts, treesRight ); // ...and right tree is subsequence
		break;
	    default:
		ts.append( right );                       // ...and right tree is atom
	    }
	    return treeListToSequence( left.pos, ts );

	default:                                          // left tree is atom
	    switch( right ) {
	    case Sequence( Tree[] treesRight ):
		TreeList ts = new TreeList();
		ts.append( left );
		appendNonEmpty( ts, treesRight ); // ...and right tree is subsequence
		return treeListToSequence( left.pos, ts );
	    }
	}
	return null;                                      // ...and right tree is atom -- no merge
    }



    /* (4) If `Alternative' at least one subsequence branch, then we wrap every atom that may
     *        occur in a `Sequence' tree
     *       (( tree traversal ))
     */

    /** applies `warpAlternative' to each tree is ts
     */
    public Tree[] wrapAlternatives( Tree[] trees ) {
	Tree[] newts = new Tree[ trees.length ];
	for( int i = 0; i < trees.length; i++ )
	    newts[ i ] = wrapAlternative( trees[ i ] );
	return newts;
    }

    /** main algo for (4)
     */
     public Tree wrapAlternative( Tree tree ) {
            switch( tree ) {
            case Alternative( Tree[] choices ):
                  return make.Alternative( tree.pos, wrapAlternativeChildren( choices ));
                  // recursive
            case Sequence( Tree[] trees ):
                  return make.Sequence( tree.pos, wrapAlternatives( trees ));
            case Bind(Name var, Tree body ):
                  return make.Bind( tree.pos, var, wrapAlternative( body ));

            case Ident( Name name ):
                  /*
                  System.out.println( "in case Ident, name" +name);
                  if ( name != Name.fromString("_")
                       && ( boundVars.get( tree.symbol() ) == null )) {

                        System.out.println("TRANSF, name:"+name);

                        return make.Bind( tree.pos,
                                          name,
                                          make.Ident( tree.pos,
                                                      Name.fromString("_") ))
                              .symbol( tree.symbol() )
                              .type( tree.type );
                  }
                  */
              return tree;

	default:
	    return tree;

	}
    }

    /** algo for (4), precondition: choices are direct successors of an `Alternative' node
     */
    Tree[] wrapAlternativeChildren( Tree[] choices ) {
	Tree[] newchoices = new Tree[ choices.length ];


	for( int i = 0; i < choices.length; i++ )
	    newchoices[ i ] = wrapAlternative( choices[ i ] ); // recursive call

	if( hasSequenceBranch( newchoices ))
	    for( int i = 0; i < choices.length; i++ )
		newchoices[ i ] = wrapElement( choices[ i ] );
	return newchoices;
    }

    /** returns true if at least one tree in etrees is a subsequence value
     */
    boolean hasSequenceBranch( Tree[] trees ) {
	boolean isSubseq = false;
	for( int i = 0; i < trees.length && !isSubseq; i++ )
	    isSubseq |= isSequenceBranch( trees[ i ] );
	return isSubseq;
    }

    /*  returns true if the argument is a subsequence value, i.e. if
     *  is is labelled `Sequence' or  a `Bind' or an `Alternative' with a `Sequence' node below
     *  precondition: choices are in normal form w.r.t. to (4)
     */
    boolean isSequenceBranch( Tree tree ) {
	switch( tree ) {
	case Sequence( _ ):
	    return true;
	case Alternative( Tree[] trees ): // normal form -> just check first child
	    switch( trees[ 0 ] ) {
	    case Sequence( _ ):
		return true;
	    default:
		return false;
	    }
	case Bind( _, Tree body ):
	    return isSequenceBranch( body );
	default:
	    return false;
	}
    }

    Tree wrapElement( Tree tree ) {
	switch( tree ) {
	case Sequence(_):
	    return tree;
	default:
	    return make.Sequence(tree.pos, new Tree[] { tree } );
	}
    }

}
