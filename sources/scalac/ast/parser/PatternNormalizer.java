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


    /** checks whether
     *  - subsequences appear properly below sequence nodes only.
     *  - @-recursion occurs only at the end just below a sequence
     *  returns true if the tree is ok.
     *  inSeq: flag, true if we are in a sequence '[' ... ']'
     *  t: the tree to be checked
     */
    protected boolean check( Tree t, boolean inSeq ) {
	switch( t ) {

	case Literal( _ ):
	    return true;

	case Apply( _, Tree[] args ):
	    return check( args, true ); // could be in sequence !

	case Sequence( Tree[] trees ):
	    return check( trees, true ); // in seq now

	case Subsequence( Tree[] trees ):
	    //System.out.println( t.toString() + inSeq ); // CHECK
	    if( !inSeq ) {
		unit.error( t.pos ,
			    "subsequence without surrounding '[' ']'");
		return false;
	    }
	    return check( trees, inSeq );

	case Alternative( Tree[] trees ):
	    //System.out.println( t.toString() );
	    return check( trees, inSeq );

	case Bind( Name var, Tree tree ):
              this.boundVars.put( t.symbol(), Boolean.FALSE );
              /*
              boolean result = check( tree, inSeq );
              if(((Boolean) this.boundVars.get( t.symbol() ))
                 .booleanValue()) { // occurs recursively
                    // do something to RESTRICT recursion
              }
              */
              return check( tree, inSeq );

	case Typed( _, _):
	    return true;

	case Ident( _ ):
              /*
              System.out.println( t.symbol().toString() );
              */
              if( this.boundVars.containsKey( t.symbol() )) {
                    this.boundVars.put( t.symbol(), Boolean.TRUE );
                    //System.out.println( t.symbol() + "occurs recursively");
              }

	    return true;

	case Select( _, _ ):
	    return true;

            /*
	case Empty: // may appear just as Subsequence Subsequence
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
      protected boolean check( Tree[] trees, boolean inSeq ) {
	for( int i = 0; i < trees.length; i++ )
	    if( !check(  trees[ i ], inSeq ))
		return false;
	return true;
    }

      // if this map contains a symbol as a key, it is bound.
      // if this symbol is mapped to Boolean.True, it occurs recursively
      HashMap boundVars;

      /**  method
       */
      public boolean check( Tree pat ) {
            this.boundVars = new HashMap();
            return check( pat, false );
    }


    //
    /////////////// NORMALIZING patterns /////////////////////////////////////////////////////////////
    //

    boolean isEmptySubsequence( Tree tree ) {
	switch( tree ) {
	case Subsequence( Tree[] trees ):
	    //return ((trees.length == 1)&&( trees[ 0 ] == Tree.Empty ));
	    return trees.length == 0;
	default:
	    return false;
	}
    }

    boolean isSubsequence( Tree tree ) {
	switch( tree ) {
	case Subsequence( _ ):
	    return true;
	default:
	    return false;
	}
    }


    /** appends every non-empty tree in trees to ts
     */
    void appendNonEmpty( TreeList ts, Tree[] trees ) {
	for( int i = 0; i < trees.length; i++ )
	    if( !isEmptySubsequence( trees[ i ] ) )
		ts.append( trees[ i ] );
    }
    /** appends tree to ts if it is non-empty, leaves ts unchanged.
     */
    void appendNonEmpty( TreeList ts, Tree tree ) {
	//if( tree != Tree.Empty )
	if ( !isEmptySubsequence(tree))
	    ts.append( tree );
    }

    /** takes a (possibly empty) TreeList and make a Subsequence out of it
     */
    Tree treeListToSubsequence( int pos, TreeList ts ) {
	if( ts.length() == 0 )
              return make.Subsequence( pos, Tree.EMPTY_ARRAY);
	return make.Subsequence( pos, ts.toArray() );
    }



    /** (1) nested `Alternative' nodes are flattened (( tree traversal with clique contraction ))
     *       if all branches are empty, empty subsequence is returned.
     *       variables x are replaced by bindings x @ _
     */

    // apply `flattenAlternative' to each tree in ts
    Tree[] flattenAlternatives( Tree[] ts ) {
	Tree[] res = new Tree[ ts.length ];
	for( int i = 0; i < ts.length; i++ )
	    res[ i ] = flattenAlternative( ts[ i ] );
	return res;
    }

    // main algo for (1)
    Tree flattenAlternative( Tree tree ) {
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
		return make.Subsequence( tree.pos, Tree.EMPTY_ARRAY );
	    default:
		return make.Alternative( tree.pos, cs.toArray() );
	    }

	    // recursive call
	case Sequence( Tree[] trees ):
	    return make.Sequence( tree.pos, flattenAlternatives( trees ) );
	case Subsequence( Tree[] trees):
	    return make.Subsequence( tree.pos, flattenAlternatives( trees ));
	case Bind( Name var, Tree body ):
	    return make.Bind( tree.pos, var, flattenAlternative( body ));
	default:
	    return tree; // no alternatives can occur
	}
    }

    // main algo for (1), precondition: choices are children of an Alternative node
    TreeList flattenAlternativeChildren( Tree[] choices ) {
	TreeList cs = new TreeList();
	for( int j = 0; j < choices.length; j++ ) {
	    Tree tree = flattenAlternative( choices[ j ] ); // flatten child
	    switch( tree ) {
	    case Alternative( Tree[] child_choices ):
		appendNonEmpty( cs, child_choices );
		break;
	    default:
		appendNonEmpty( cs, tree );
	    }
	}
	//System.out.println( cs.length() );
	return cs;
    }



    /** (2) nested `Subsequence' nodes are flattened (( clique elimination ))
     *      nested empty subsequences get deleted
     */

    // apply `flattenSubsequence' to each tree in trees
    Tree[] flattenSubsequences( Tree[] trees ) {
	Tree[] res = new Tree[ trees.length ];
	for( int i = 0; i < trees.length; i++ )
	    res[ i ] = flattenSubsequence( trees[ i ] );
	return res;
    }
    // main algo for (2)
    Tree flattenSubsequence( Tree tree ) {
	switch( tree ) {
	case Sequence( Tree[] trees ):
	    trees = flattenSubsequences( trees );
	    if(( trees.length == 1 )&&( isEmptySubsequence( trees[ 0 ] )))
		trees = Tree.EMPTY_ARRAY;
	    return make.Sequence( tree.pos, trees );
	case Subsequence( Tree[] trees ):
	    TreeList ts = new TreeList();
	    for( int i = 0; i < trees.length; i++ ) {
		Tree child = trees[ i ];
		switch( child ) {
		case Subsequence( Tree[] child_trees ): // grab its flattened children
		    ts.append( flattenSubsequenceChildren( child_trees ) );
		    break;
		default:
		    ts.append( child );
		}
	    }
	    return treeListToSubsequence( tree.pos, ts );
	    // recursive call
	case Alternative( Tree[] choices ):
	    return make.Alternative( tree.pos, flattenSubsequences( choices ));
	case Bind( Name var, Tree body ):
	    return make.Bind( tree.pos, var, flattenSubsequence( body ));
	default:
	    return tree;
	}
    }

    // main algo for (2), precondition: trees are children of a Subsequence node
    TreeList flattenSubsequenceChildren( Tree[] trees ) {
	TreeList ts = new TreeList();
	for( int j = 0; j < trees.length; j++ ) {
	    Tree tree = flattenSubsequence( trees[ j ] );
	    switch( tree ) {
	    case Subsequence( Tree[] child_trees ):
		appendNonEmpty( ts, child_trees );
		break;
	    default:
		appendNonEmpty( ts, tree );
	    }
	}
	return ts;
    }


    /** (3) in `Sequence':
     *             children of direct successor nodes labelled `Subsequence' are moved up
     *      (( tree traversal, left-to-right traversal ))
     */

    /** applies `elimSubsequence' to each tree is ts
     */
    Tree[] elimSubsequences( Tree[] ts ) {
	Tree[] res = new Tree[ ts.length ];
	for( int i = 0; i < ts.length; i++ )
	    res[ i ] = elimSubsequence( ts[ i ] );
	return res;
    }

    Tree elimSubsequence( Tree tree ) {
	switch( tree ) {
	case Sequence( Tree[] trees ):
	    // might be empty ...
	    Tree[] newtrees = mergeHedge( trees ).toArray();
	    if(( newtrees.length == 1 )&&( isEmptySubsequence( newtrees[ 0 ] )))
		return make.Sequence( tree.pos, Tree.EMPTY_ARRAY );
	    return make.Sequence( tree.pos, newtrees );

	    // recurse
	case Subsequence( Tree[] trees ): // after normalization (2), Subsequence is flat
	    /*
	    TreeList ts = new TreeList();
	    for( int i = 0; i < trees.length; i++ ) {
		Tree t = trees[ i ];
		//if( t != Tree.Empty )                          // forget empty subsequences
		    ts.append( elimSubsequence( t )); // recurse
	    }
	    return treeListToSubsequence( tree.pos, ts );
	    */
	    return make.Subsequence( tree.pos, elimSubsequences( trees ));
	case Alternative( Tree[] choices ):
	    Tree result = make.Alternative( tree.pos, elimSubsequences( choices ) );
	    return flattenAlternative( result ); // apply
	case Bind( Name var, Tree body ):
	    return make.Bind( tree.pos, var, elimSubsequence( body ));
	default:
	    return tree; // nothing to do
	}
    }

    /** runs through an array of trees, merging adjacent `Subsequence' nodes if possible
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
	case Subsequence( Tree[] treesLeft ):             // left tree is subsequence
	    TreeList ts = new TreeList();
	    appendNonEmpty( ts, treesLeft );
	    switch( right ) {
	    case Subsequence( Tree[] treesRight ):
		appendNonEmpty( ts, treesRight ); // ...and right tree is subsequence
		break;
	    default:
		ts.append( right );                       // ...and right tree is atom
	    }
	    return treeListToSubsequence( left.pos, ts );

	default:                                          // left tree is atom
	    switch( right ) {
	    case Subsequence( Tree[] treesRight ):
		TreeList ts = new TreeList();
		ts.append( left );
		appendNonEmpty( ts, treesRight ); // ...and right tree is subsequence
		return treeListToSubsequence( left.pos, ts );
	    }
	}
	return null;                                      // ...and right tree is atom -- no merge
    }



    /* (4) If `Alternative' at least one subsequence branch, then we wrap every atom that may
     *        occur in a `Subsequence' tree
     *       (( tree traversal ))
     */

    /** applies `warpAlternative' to each tree is ts
     */
    Tree[] wrapAlternatives( Tree[] trees ) {
	Tree[] newts = new Tree[ trees.length ];
	for( int i = 0; i < trees.length; i++ )
	    newts[ i ] = wrapAlternative( trees[ i ] );
	return newts;
    }

    /** main algo for (4)
     */
      Tree wrapAlternative( Tree tree ) {
            switch( tree ) {
            case Alternative( Tree[] choices ):
                  return make.Alternative( tree.pos, wrapAlternativeChildren( choices ));
                  // recursive
            case Sequence( Tree[] trees ):
                  return make.Sequence( tree.pos, wrapAlternatives( trees ));
            case Subsequence( Tree[] trees ):
                  return make.Subsequence( tree.pos, wrapAlternatives( trees ));
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

	if( hasSubsequenceBranch( newchoices ))
	    for( int i = 0; i < choices.length; i++ )
		newchoices[ i ] = wrapElement( choices[ i ] );
	return newchoices;
    }

    /** returns true if at least one tree in etrees is a subsequence value
     */
    boolean hasSubsequenceBranch( Tree[] trees ) {
	boolean isSubseq = false;
	for( int i = 0; i < trees.length && !isSubseq; i++ )
	    isSubseq |= isSubsequenceBranch( trees[ i ] );
	return isSubseq;
    }

    /*  returns true if the argument is a subsequence value, i.e. if
     *  is is labelled `Subsequence' or  a `Bind' or an `Alternative' with a `Subsequence' node below
     *  precondition: choices are in normal form w.r.t. to (4)
     */
    boolean isSubsequenceBranch( Tree tree ) {
	switch( tree ) {
	case Subsequence( _ ):
	    return true;
	case Alternative( Tree[] trees ): // normal form -> just check first child
	    switch( trees[ 0 ] ) {
	    case Subsequence( _ ):
		return true;
	    default:
		return false;
	    }
	case Bind( _, Tree body ):
	    return isSubsequenceBranch( body );
	default:
	    return false;
	}
    }

    Tree wrapElement( Tree tree ) {
	switch( tree ) {
	case Subsequence(_):
	    return tree;
	default:
	    return make.Subsequence(tree.pos, new Tree[] { tree } );
	}
    }

}
