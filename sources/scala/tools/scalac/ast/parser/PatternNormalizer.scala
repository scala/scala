/*                    __                                             *\
**    ______________ _/ /___ ______                                  **
**   / ___/ ___/ __ `/ / __ `/ ___/ (c) 2002-2004, LAMP/EPFL         **
**  (__  ) /__/ /_/ / / /_/ / /__                                    **
** /____/\___/\__,_/_/\__,_/\___/                                    **
**                                                                   **
** $Id$                                                              **
\*                                                                   */

import scalac.ast._;
import scalac._;
import scala.Iterator;
import scala.tools.scalac.util.NewArray;
import scala.collection.immutable.ListMap ;
import scala.collection.mutable.Buffer;

import java.util.HashMap;
import scalac.util.Names;

/*
import scalac.Unit;
import scalac.ast.*;
import scalac.util.Name;
import Tree.*;
*/

package scala.tools.scalac.ast.parser {

  import scala.tools.scalac.ast.{TreeList => myTreeList}

  /** contains algorithms for `checking' and `normalizing' patterns
  *
  *  @author  Burak Emir
  *  @version 1.0
  */

  class PatternNormalizer( unit:Unit ) {

    val make = unit.global.make ;

    //
    ///////// CHECKING patterns for well-formedness ///////////////////////////////
    //

    var seqDepth:int = 0;

    /** checks whether TO DO TO DO TO DO
     *  - @-recursion occurs only at the end just below a sequence
     *  returns true if the tree is ok.
     *  inSeq: flag, true if we are in a sequence '[' ... ']'
     *  t: the tree to be checked
     */
    protected def check1( t:Tree , inAlt:boolean  ):boolean = {
      t match {

	case Tree$Literal( _ ) => true;

	case Tree$Apply( _, args ) =>
              seqDepth = seqDepth + 1;
              val res = check1( args, inAlt );
              seqDepth = seqDepth - 1;
              res;

	case Tree$Sequence( trees ) => // this is a hack to disallow deep binding
              seqDepth = seqDepth + 1;
              val res = check1( trees, inAlt );
              seqDepth = seqDepth - 1;
              res;

	case Tree$Alternative( trees ) =>
	    check1( trees, true );

	case Tree$Bind( vble, tree ) =>
	  if(( inAlt )&&( vble.toString().lastIndexOf("$") == -1)) {

	    unit.error( t.pos,  "variable binding not allowed under alternative");
	    false;

	  } else if(( seqDepth > 2 )&&( vble.toString().lastIndexOf("$") == -1)) {
	    unit.error( t.pos,
		       "sorry, deep binding not implemented");
	    return false;
          } else {
	    this.boundVars.put( vble, false );
	    /*
              boolean result = check( tree, inSeq );
              if(((Boolean) this.boundVars.get( t.symbol() ))
	      .booleanValue()) { // occurs recursively
	      // do something to RESTRICT recursion
              }
	    */
	    check1( tree, inAlt );
	  }
	case Tree$Typed( _, _ ) => true

	case Tree$Ident( vble ) =>
	  if (inAlt && vble.isVariable() && vble != Names.PATTERN_WILDCARD &&
	      vble.lastIndexOf('$') == -1) {
		unit.error( t.pos, "variable not allowed under alternative");
		return false;
	      } else if(( this.boundVars.containsKey( vble /*t.symbol()*/ ))
	              &&( vble.toString().lastIndexOf("$") == -1)) {
		        unit.error( t.pos, "recursive patterns not allowed");
		        false
		        //this.boundVars.put( t.symbol(), Boolean.TRUE ); //mark recursive
                        //System.out.println( t.symbol() + "occurs recursively");
                      }
              else true;

	case Tree$Select( _, _ ) => true;

	case _ => /* never happens */
          unit.error( t.pos, "unexpected tree" + t );
          false
      }
    }

    /** checkPat for every tree in array of trees, see below
    */
    protected def check1( trees:Array[Tree], inAlt:boolean ):boolean = {
      var res = true;
      var i:int = 0; while (( i < trees.length ) && res) {
	res = check1(  trees( i ), inAlt );
        i = i + 1;
      }
      res
    }

    // if this map contains a symbol as a key, it is bound.
    // if this symbol is mapped to Boolean.True, it occurs recursively
    /*Name=>Boolean*/ var boundVars:HashMap = _;

    /**  method
    */
    def check( pat:Tree ):boolean = {
      this.boundVars = new HashMap();

      //reject top-level sequence patterns
      pat match {
        case Tree$Sequence( _ ) =>
          unit.error( pat.pos, "sequences not allowed here"); false;

        case Tree$Alternative( args ) =>
          var res = true;
          var i = 0; while( res && (i < args.length )) {
            args( i ) match  {
              case Tree$Sequence( _ ) =>
                unit.error( args( i ).pos, "sequences not allowed here");
                res = false;
              case _ =>
            };
            i = i + 1
          }
        case _ =>
      }
      // reject deep binding
      if( TreeInfo.isRegularPattern( pat ) )
        this.seqDepth = 0;
      else
        this.seqDepth = -32;  // don't care about sequences. see above
      check1( pat, false );
    }


    //
    /////////////// NORMALIZING patterns /////////////////////////////////////////////////////////////
    //

    def isEmptySequence( tree:Tree ):boolean = {
      tree match {
	case Tree$Sequence( trees ) =>
	  //return ((trees.length == 1)&&( trees[ 0 ] == Tree.Empty ));
	  trees.length == 0;
	case _ =>
	  false;
      }
    }

    def isSequence( tree:Tree ):boolean = {
      tree match {
	case Tree$Sequence( _ ) => true
	case _ => false;
      }
    }

    /** appends every non-empty tree in trees to ts
    */
    def appendNonEmpty( ts:myTreeList, trees:Array[Tree] ):unit = {
      var i = 0; while ( i < trees.length ) {
	if( !isEmptySequence( trees( i ) ) )
	  ts.append( trees( i ) );
        i = i + 1
      }
    }

    /** appends tree to ts if it is non-empty, leaves ts unchanged.
    */
    def appendNonEmpty( ts:myTreeList, tree:Tree  ) = {
      if ( !isEmptySequence(tree))
	ts.append( tree );
    }

    /** takes a (possibly empty) myTreeList and make a Sequence out of it
    */
    def treeListToSequence( pos:int , ts:myTreeList  ):Tree = {
      make.Sequence( pos, ts.toArray() );
    }


    /** (1) nested `Alternative' nodes are flattened (( tree traversal with clique contraction ))
    *       if all branches are empty, empty subsequence is returned.
    *       variables x are replaced by bindings x @ _
    */

    // apply `flattenAlternative' to each tree in ts
    def flattenAlternatives( ts:Array[Tree] ):Array[Tree] = {
      val res = new Array[Tree]( ts.length );
      var i = 0; while( i < ts.length ) {
	res( i ) = flattenAlternative( ts( i ) );
        i = i + 1 ;
      }
      res;
    }

    // main algo for (1)
    def flattenAlternative( tree:Tree ):Tree = {
      tree match  {
	case Tree$Alternative( choices:Array[Tree]  ) =>
	  val cs = new myTreeList();
	  var i = 0; while( i < choices.length ) {
	    val child = choices( i );
	    child match {
	      case Tree$Alternative( child_choices ) => // grab its flattened children
	        cs.append( flattenAlternativeChildren( child_choices ) );
	      case _ =>
	        cs.append( child );
	    }
            i = i + 1;
	  }
	  val newtrees = cs.toArray();
	  newtrees.length match {
	    case 1 => newtrees( 0 );
	    case 0 => make.Sequence( tree.pos, Tree.EMPTY_ARRAY );
	    case _ => make.Alternative( tree.pos, cs.toArray() );
	  }

	// recursive call
	case Tree$Sequence( trees) =>
	  make.Sequence( tree.pos, flattenAlternatives( trees ));
	case Tree$Bind( vble, body ) =>
	  make.Bind( tree.pos, vble, flattenAlternative( body ));
	case _ =>
	    tree; // no alternatives can occur
      }
    }

    // main algo for (1), precondition: choices are children of an Alternative node
    def flattenAlternativeChildren( choices:Array[Tree] ):myTreeList = {
      var allEmpty = true;
      val cs = new myTreeList();
      var j = 0; while( j < choices.length ) {
	val tree = flattenAlternative( choices( j ) ); // flatten child
	tree match {
	  case Tree$Alternative( child_choices:Array[Tree] ) =>
	    val tmp = cs.length();
	    appendNonEmpty( cs, child_choices );
	    if( cs.length() != tmp )
	      allEmpty = false;
	  case _ =>
	    cs.append( tree );
	    allEmpty = allEmpty && TreeInfo.isEmptySequence( tree );
	}
        j = j + 1;
      }
      if( allEmpty ) {
	cs.clear();
	cs.append(make.Sequence(choices( 0 ).pos, Tree.EMPTY_ARRAY));
      }
      cs
    }

    /** (2) nested `Sequence' nodes are flattened (( clique elimination ))
     *      nested empty subsequences get deleted
     */

    // apply `flattenSequence' to each tree in trees
    def flattenSequences( trees:Array[Tree] ):Array[Tree] = {
      val res = new Array[Tree]( trees.length );
      var i = 0;while( i < trees.length ) {
	res( i ) = flattenSequence( trees( i ) );
        i = i + 1;
      }
      res
    }
    // main algo for (2)
    def flattenSequence( tree:Tree  ):Tree = {
      tree match {
	/*
	case Sequence( Tree[] trees ):
	trees = flattenSequences( trees );
	if(( trees.length == 1 )&&( isEmptySequence( trees[ 0 ] )))
	  trees = Tree.EMPTY_ARRAY;
	return make.Sequence( tree.pos, trees );
	*/
	case Tree$Sequence( trees:Array[Tree] ) =>
	  val ts = new myTreeList();
	  var i = 0; while( i < trees.length ) {
	    val child = trees( i );
	    child match {
	      case Tree$Sequence( child_trees  ) => // grab its flattened children
	        ts.append( flattenSequenceChildren( child_trees ) );
	      case _ =>
		ts.append( child );
	    }
            i = i + 1;
	  }
	    /*
	      System.out.print("ts = ");
	    for(int jj = 0; jj<ts.length(); jj++) {
		System.out.print(ts.get( jj ).toString()+" ");
	    }
	    System.out.println();
	    */
	val t = treeListToSequence( tree.pos, ts ) ;
	t
	  case Tree$Alternative( choices ) =>
	    make.Alternative( tree.pos, flattenSequences( choices ));
	  case Tree$Bind( vble, body ) =>
	    make.Bind( tree.pos, vble, flattenSequence( body ));
	  case _ => tree;
	}
    }

    // main algo for (2), precondition: trees are children of a Sequence node
    def flattenSequenceChildren( trees:Array[Tree] ):myTreeList = {
      val ts = new myTreeList();
      var j = 0; while( j < trees.length ) {
	val tree = flattenSequence( trees( j ) );
	tree match  {
	  case Tree$Sequence( child_trees ) =>
	    appendNonEmpty( ts, child_trees );
	  case _ =>
	    appendNonEmpty( ts, tree );
	}
        j = j + 1;
      }
      ts;
    }


    /** (3) in `Sequence':
     *             children of direct successor nodes labelled `Sequence' are moved up
     *      (( tree traversal, left-to-right traversal ))
     */

    /** applies `elimSequence' to each tree is ts
     */
    def elimSequences( ts:Array[Tree]  ):Array[Tree] = {
	val res = new Array[Tree] ( ts.length );
	var i = 0; while( i < ts.length ) {
	    res( i ) = elimSequence( ts( i ) );
            i = i + 1
        }
	res;
    }

    def elimSequence( tree:Tree  ):Tree = {
	tree match {
	  case Tree$Sequence( trees ) =>
	    // might be empty ...
	    val newtrees = mergeHedge( trees ).toArray();
	    if(( newtrees.length == 1 )&&( isEmptySequence( newtrees( 0 ) )))
	      make.Sequence( tree.pos, Tree.EMPTY_ARRAY );
	    else
              make.Sequence( tree.pos, newtrees );

	    // recurse
	    //case Sequence( Array[Tree] trees ): // after normalization (2), Sequence is flat
	    /*
	    myTreeList ts = new myTreeList();
	    for( int i = 0; i < trees.length; i++ ) {
		Tree t = trees[ i ];
		//if( t != Tree.Empty )                          // forget empty subsequences
		    ts.append( elimSequence( t )); // recurse
	    }
	    return treeListToSequence( tree.pos, ts );
	    */
	    //return make.Sequence( tree.pos, elimSequences( trees ));
	  case Tree$Alternative( choices ) =>
	    val result = make.Alternative( tree.pos, elimSequences( choices ) );
	    flattenAlternative( result ); // apply
	  case Tree$Bind( vble, body ) =>
	    make.Bind( tree.pos, vble, elimSequence( body ));
	  case _ =>
	    tree; // nothing to do
	}
    }

    /** runs through an array of trees, merging adjacent `Sequence' nodes if possible
     *  returns a (possibly empty) myTreeList
     *  precondition: trees are children of a Sequence node
     */
    def mergeHedge( trees:Array[Tree] ):myTreeList = {
      val ts = new myTreeList();
      if( trees.length > 1 ) {    // more than one child
	var left  = trees( 0 );
	var right:Tree = null;;
	var merge:Tree = null;
	var i = 0; while( i < trees.length - 1 ) {
	  right = trees( i + 1 );
	  merge = mergeThem( left, right );
	  if( merge != null ) {
	    left = merge;
	  } else {
	    ts.append( left );
	    left = right;
	  }
          i = i + 1;
	}
	if( merge!= null ) {
	  ts.append( merge );
	} else if( right != null ) {
	  ts.append( right );
        }
      } else if ( trees.length == 1 ) {
        //if( trees[ 0 ] != Tree.Empty )
	ts.append( trees( 0 ) ); // append the single child
      }
      ts;
    }

    /** if either left or right are subsequences, returns a new subsequences node
     *  with the children of both, where any occurences of Tree.Empty are removed
     *  otherwise, returns null. "move concatenation to the top"
     */
    def mergeThem( left:Tree, right:Tree  ):Tree = {
      left match {
	case Tree$Sequence( treesLeft ) =>             // left tree is subsequence
	  val ts = new myTreeList();
	  appendNonEmpty( ts, treesLeft );
	  right match  {
	    case Tree$Sequence( treesRight ) => // subsequence
	      appendNonEmpty( ts, treesRight );
	    case _ => // atom
	      ts.append( right );
	  };
	  treeListToSequence( left.pos, ts );

	case _ =>                                          // left tree is atom
	  right match {
	    case Tree$Sequence( treesRight ) =>
	      val ts = new myTreeList();
	      ts.append( left );
	      appendNonEmpty( ts, treesRight ); // ...and right tree is subsequence
	      treeListToSequence( left.pos, ts );
            case _ => //  right tree is atom -- no merge
              null
	  }
      }
    }



    /* (4) If `Alternative' at least one subsequence branch, then we wrap every atom that may
     *        occur in a `Sequence' tree
     *       (( tree traversal ))
     */

    /** applies `warpAlternative' to each tree is ts
     */
    def wrapAlternatives( trees:Array[Tree] ):Array[Tree] = {
      val newts = new Array[Tree]( trees.length );
      var i = 0; while( i < trees.length ) {
	newts( i ) = wrapAlternative( trees( i ) );
        i = i + 1;
      }
      newts;
    }

  /** main algo for (4)
    */
  def wrapAlternative( tree:Tree  ):Tree = {
    tree match {
      case Tree$Alternative( choices ) =>
        make.Alternative( tree.pos, wrapAlternativeChildren( choices ));
      // recursive
      case Tree$Sequence( trees ) =>
        make.Sequence( tree.pos, wrapAlternatives( trees ));
      case Tree$Bind( vble, body ) =>
        make.Bind( tree.pos, vble, wrapAlternative( body ));

      case Tree$Ident( name ) =>
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
        tree;

      case _ =>
	tree;

    }
  }

  /** algo for (4), precondition: choices are direct successors of an `Alternative' node
  */
  def wrapAlternativeChildren( choices:Array[Tree] ):Array[Tree] = {
    val newchoices = new Array[Tree]( choices.length );

    var i = 0;while( i < choices.length ) {
      newchoices( i ) = wrapAlternative( choices( i ) ); // recursive call
      i = i + 1;
    }

    if( hasSequenceBranch( newchoices )) {
      var i = 0; while( i < choices.length ) {
	newchoices( i ) = wrapElement( choices( i ) );
        i = i + 1;
      }
    }
    newchoices
  }

  /** returns true if at least one tree in etrees is a subsequence value
  */
    def hasSequenceBranch( trees:Array[Tree] ):boolean = {
      var isSubseq = false;
      var i = 0; while( i < trees.length && !isSubseq ) {
	isSubseq = isSequenceBranch( trees( i ) );
        i = i + 1;
      }
      isSubseq;
    }

    /*  returns true if the argument is a subsequence value, i.e. if
     *  is is labelled `Sequence' or  a `Bind' or an `Alternative' with a `Sequence' node below
     *  precondition: choices are in normal form w.r.t. to (4)
     */
    def isSequenceBranch( tree:Tree  ):boolean = {
	tree match {
	case Tree$Sequence( _ ) =>
	    true;
	  case Tree$Alternative( trees:Array[Tree]  )=> // normal form -> just check first child
	    trees( 0 ) match  {
	    case Tree$Sequence( _ ) => true;
	    case _ => false;
	    }
	case Tree$Bind( _, body ) =>
	    isSequenceBranch( body );
	case _ =>
	  false;
	}
    }

    def wrapElement( tree:Tree ):Tree = {
      tree match {
        case Tree$Sequence(_) =>
	  tree;
        case _ =>
	  make.Sequence(tree.pos, {val t = new Array[Tree]( 1 );t(0) = tree; t } );
      }
    }

  } /*class PatternNormalizer */

} /* package */

