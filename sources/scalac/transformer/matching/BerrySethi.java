package scalac.transformer.matching ;

import scalac.ApplicationError ;
import scalac.ast.Tree ;
import scalac.util.Name ;
import Tree.* ;

import java.util.* ;

//import scala.compiler.printer.TextTreePrinter ;
//import scala.compiler.printer.XMLAutomPrinter ;

/** a Berry-Sethi style construction for nfas.
 *  this class plays is the "Builder" for the "Director" class WordRecognizer.
 */

class BerrySethi {

      boolean isStar( Name n ) {
            String s = n.toString();
            return (s.indexOf("$") != -1)
                  &&(!s.startsWith("nest"));
      }


      HashSet labels;

      int pos;
      // maps a literal pattern to an Integer ( the position )
      // is not *really* needed (postfix order determines position!)
      HashMap posMap;    // pos:    Patterns   -> Positions
      // don't let this fool you, only labelAt is a real, surjective mapping
      HashMap labelAt; // chi:    Positions -> Obj

      TreeSet globalFirst;

      // results which hold all info for the NondetWordAutomaton

      HashMap follow;    // follow: Positions -> Set[Positions]


      // Unit test ?
      boolean nullable( Tree pat ) {
            //System.out.print("<nullable>");
            //DEBUG.print( pat );
            //System.out.println("</nullable>");
            switch( pat ) {
            case Apply(_, _):
                  return false;
            case Sequence( Tree[] trees ):
                  return (trees.length == 0) || nullable( trees );
		  //case Subsequence( Tree[] trees ):
                  //return
            case Bind(Name n, Tree t):
                  /*
                     if( isStar( n ) ) // generated for star/plus(?)
                     return true;
                  */
                  return nullable( t );
            case Alternative(Tree[] choices):
                  boolean result = false;
                  for( int i = 0; i < choices.length && !result; i++ )
                        result = result || nullable( choices[ i ] );
                  return result;
            default:
                  return false;

            }
      }


      /** returns true if a Sequence pattern matches the empty sequence
       *  @param pat the sequence pattern.
       */
      boolean nullableSequence( Tree pat ) {
            switch( pat ) {
            case Sequence( Tree[] pats ):
                  return nullable( pats );
            }
            return false;
      }

      /** returns true if a sequence of patterns (usually children of a
       *  sequence or subsequence node) is nullable.
       *  @param pats the sequence of patterns
       */
      boolean nullable( Tree[] pats ) {
            boolean result = true;
            for( int i = 0; i < pats.length && result; i++ )
                  result = result && nullable( pats[ i ] );
            return result;
      }

      /** computes first( alpha ) where alpha is a word regexp
       */

      TreeSet compFirst( Tree pat ) {
            //System.out.print("<compFirst>");
            //DEBUG.print( pat );
            //System.out.println("</compFirst>");
            switch( pat ) {
            case Sequence( _ ):
            case Typed(_,_):
            case Select(_,_):
            case Apply(_, _):
                  TreeSet tmp = new TreeSet();
                  tmp.add( (Integer) posMap.get( pat )); // singleton set
                  return tmp;
            case Literal( _ ):
                  TreeSet tmp = new TreeSet();
                  tmp.add( (Integer) posMap.get( pat )); // singleton set
                  return tmp;
		  //case Subsequence( Tree[] trees ):
                  //return compFirst( trees );
            case Alternative( Tree[] trees ):
                  TreeSet tmp = new TreeSet();
                  for( int i = 0; i < trees.length; i++ ) {
                        tmp.addAll( compFirst( trees[ i ] ));
                  }
                  return tmp;
            case Bind( _, Tree tree ):
                  return compFirst( tree );
            case Ident( Name name ):
                  //if( name != Name.fromString("_") )
                  //    throw new ApplicationError("unexpected pattern");
                  TreeSet tmp = new TreeSet();
                  tmp.add( (Integer) posMap.get( pat )); // singleton set
                  return tmp;
            default:
                  throw new ApplicationError("unexpected pattern");
            }
      }



      /** computes last( alpha ) where alpha is a word regexp
       */
      TreeSet compLast( Tree pat ) {
            //System.out.print("<last>");
            //DEBUG.print( pat );
            //System.out.println("</compLast>");
            switch( pat ) {
            case Sequence( _ ):
            case Apply(_, _):
                  TreeSet tmp = new TreeSet();
                  tmp.add( (Integer) posMap.get( pat )); // singleton set
                  return tmp;
            case Literal( _ ):
                  TreeSet tmp = new TreeSet();
                  tmp.add( (Integer) posMap.get( pat )); // singleton set
                  return tmp;
		  //case Subsequence( Tree[] trees ):
                  //return compLast( trees );
            case Alternative( Tree[] trees ):
                  TreeSet tmp = new TreeSet();
                  for( int i = 0; i < trees.length; i++ ) {
                        tmp.addAll( compLast( trees ));
                  }
                  return tmp;
            case Bind( _, Tree tree ):
                  return compLast( tree );
            default:
                  throw new ApplicationError("unexpected pattern");
            }
      }


      /** computes first(w) where w=alpha_1...alpha_n  are successors of a
       *  sequence node
       */
      TreeSet compFirst( Tree[] pats ) {
            if( pats.length == 0 )
                  return new TreeSet();

            int i = 0;
            Tree tmp = pats[ i ];
            TreeSet result = compFirst( tmp );
            i++;
            while( nullable(tmp) && (i < pats.length )) {
                  tmp = pats[ i ];
                  result.addAll( compFirst( tmp ));
                  i++;
            }
            return result;
      }

      // Unit test ?

      /** computes last(w) where w are successors of a sequence node
       */
      TreeSet compLast( Tree[] pats ) {
            /*
              System.out.print("<last>");
            for( int k = 0; k<pats.length; k++) {
                  DEBUG.print( pats[k] );
                  System.out.print("  ");
            }
            System.out.println();
            */
            if( pats.length == 0 )
                  return new TreeSet();

            int i = pats.length - 1;
            Tree tmp = pats[ i ];
            TreeSet result = compLast( tmp );
            i--;
            while( nullable(tmp) && (i >= 0 )) {
                  tmp = pats[ i ];
                  result.addAll( compLast( tmp ));
                  i++;
            }
            return result;
      }

      // starts from the right-to-left
      // precondition: pos is final
      //               pats are successor patterns of a Sequence node
      TreeSet compFollow(Tree[] pats) {
            TreeSet first = null;
            this.recVars = new HashMap();
            TreeSet fol = new TreeSet();
            if( pats.length > 0 ) {//non-empty expr
                  int i = pats.length;
                  fol.add( new Integer( pos )); // don't modify pos !
                  do {
                        --i;
                        first = compFollow1( fol, pats[ i ] );
                        if( nullable( pats[ i ] ))
                              fol.addAll( first );
                        else
                              fol = first;
                        //System.out.println("in compFollow: first"+first);
                        //System.out.println("in compFollow: fol"+fol);

                  } while( i > 0 );
            }
            if( null == first )
                  first = new TreeSet();
            else {
                  first = fol;
            }
            this.follow.put(new Integer( 0 ), first);
            return first;
      }

      HashMap recVars;

      /** returns the first set of an expression, setting the follow set along
       *  the way
       */
      TreeSet compFollow1( TreeSet fol, Tree pat ) {
            switch( pat ) {
            case Sequence(Tree[] trees):
                  TreeSet first = null;
                  int i = trees.length;
                  if( i > 0 ) { // is nonempty
                        do {
                              --i;
                              first = compFollow1(fol, trees[ i ]);
                              if( nullable( trees[ i ] ))
                                    fol.addAll( first );
                              else
                                    fol = first;
                        } while( i > 0 ) ;
                  }
                  if( null == first ) first = new TreeSet();
                  return first;

            case Alternative(Tree[] choices):
                  TreeSet first = new TreeSet();
                  for( int i = choices.length - 1; i >= 0; --i ) {
                        first.addAll( compFollow1( fol, choices[ i ] ));
                  }
                  return first;

            case Bind( Name n, Tree t ):

                  Integer p = (Integer) this.posMap.get( pat );

                  TreeSet first = compFirst( t );
                  //System.out.print("BIND" + first);
                  recVars.put( pat.symbol(), first );

                  // if( appearsRightmost( n, t ))
                  // follow = oldfollw + ownfirst
                  if( isStar( n ) )
                        fol.addAll( first ); // an iterated pattern

                  this.follow.put( p, fol.clone() );
                  //System.out.println("Bind("+n+",...) first:"+first);
                  //System.out.println("Bind("+n+",...) follow:"+fol);

                  // continue to compute follow sets with adjusted fol
                  return compFollow1( fol, t );

            case Ident( Name n ):
                  if ((pat.symbol() != null )
                      && pat.symbol().isPrimaryConstructor()) {
                        // same as Apply
                        Integer pos = (Integer) this.posMap.get( pat );
                        TreeSet tset = (TreeSet) fol.clone();
                        this.follow.put( pos, tset );
                        TreeSet first = new TreeSet();
                        first.add( pos );
                        return first;
                  }

                  if ( recVars.keySet().contains( pat.symbol() )) { // grammar
                        TreeSet first = (TreeSet) recVars.get( pat.symbol() );
                        TreeSet follow = ((TreeSet) fol.clone());
                        first.addAll( follow );
                        //recVars.put//this.follow.put( pat.symbol(), follow );
                        return first;
                  }

                  // --- --- only happens when called from BindingBerrySethi
                  // [... x ...] changed to [... x@_ ...]

                  // non-generated, non-recursive variable should not appear,
                  // so this is a wildcard pattern _

                  Integer pos = (Integer) this.posMap.get( pat );
                  TreeSet tset = (TreeSet) fol.clone();
                  this.follow.put( pos, tset );
                  TreeSet first = new TreeSet();
                  first.add( pos );
                  //System.out.println("Ident("+n+",...) first:"+first);
                  //System.out.println("Ident("+n+",...) follow:"+tset);
                  return first;

            case Apply(_, _):
            case Literal( _ ):
            case Typed(_,_):
            case Select(_,_):
                  Integer pos = (Integer) this.posMap.get( pat );
                  TreeSet tset = (TreeSet) fol.clone();
                  this.follow.put( pos, tset );
                  TreeSet first = new TreeSet();
                  first.add( pos );
                  return first;
            default:
                  throw new ApplicationError("unexpected pattern: "+pat.getClass());
            }
      }

      /** called at the leaves of the regexp
       */
      void seenLabel( Tree pat, Integer i, Label label ) {
            this.posMap.put( pat, i );
            this.labelAt.put( i, label );
            if( label != Label.DefaultLabel )
                  this.labels.add( label );
      }

      /** overriden in BindingBerrySethi
       */
      void seenLabel( Tree pat, Label label ) {
            seenLabel( pat, new Integer( ++pos ), label );
      }

      /** returns "Sethi-length" of a pattern, creating the set of position
       *  along the way
       */

      Vector activeBinders;

      // todo: replace global variable pos with acc
      void traverse( Tree pat ) {
            switch( pat ) {

                  // (is tree automaton stuff, more than Berry-Sethi)
            case Apply( _, _ ):
            case Typed( _, _ ):
            case Select( _, _ ):
                  Label label = new Label.TreeLabel( pat );
                  seenLabel( pat, label ) ;

                  return ;

            case Literal( Object val ):
                  Label label = new Label.SimpleLabel( (Literal) pat );
                  seenLabel( pat, label ) ;

                  return ;

            case Sequence( Tree[] trees ):
                  for( int i = 0; i < trees.length; i++ ) {
                        traverse( trees[ i ] );
                  }
                  return ;
            case Alternative(Tree[] choices):
                  for( int i = 0; i < choices.length; i++ ) {
                        traverse( choices[ i ] );
                  }
                  return ;
            case Bind(Name name, Tree body):
                  recVars.put( pat.symbol(), Boolean.TRUE );
                  if( !isStar( name ) )
                        {
                              activeBinders.add( pat.symbol() );
                              traverse( body );
                              activeBinders.remove( pat.symbol() );
                        }
                  else
                        traverse( body );
                  return ;

            case Ident(Name name):
                  if ((pat.symbol() != null )
                      && pat.symbol().isPrimaryConstructor()) {
                        // same as Apply
                        Label label = new Label.TreeLabel( pat );
                        seenLabel( pat, label ) ;

                        return ;
                  }


                  if( recVars.get( pat.symbol() ) != null ) {
                        return ;
                  }
                  // _ and variable x ( == x @ _ )
                  Label label = Label.DefaultLabel;
                  seenLabel( pat, label );

                  return ;

                  //else throw new ApplicationError("cannot handle this: "+name);
                  //            case Apply( _, _ ):
                  //throw new ApplicationError("cannot handle this");
            default:
                  throw new ApplicationError("this is not a pattern");
            }
      }


      TreeMap finals;      // final states

      //TreeSet initialsRev;      // final states

      HashMap deltaq[];    // delta



      Vector  defaultq[];  // default transitions


      //HashMap deltaqRev[];    // delta of Rev
      //Vector  defaultqRev[];  // default transitions of Rev


 protected void makeTransition( Integer srcI, Integer destI, Label label ) {
            int src  = srcI.intValue() ;
            int dest = destI.intValue() ;
            Vector arrows; //, revArrows;
            //Label revLabel = new Label.Pair( srcI, label );
            switch( label ) {
            case DefaultLabel:
                  arrows = defaultq[ src ];
                  //revArrows = defaultqRev[ dest ];
                  break;
            default:
                  arrows = (Vector) deltaq[ src ].get( label );
                  if( arrows == null )
                        deltaq[ src ].put( label,
                                           arrows = new Vector() );
                  /*
                  revArrows = (Vector) deltaqRev[ dest ].get( revLabel );
                  if( revArrows == null )
                        deltaqRev[ dest ].put( revLabel,
                                               revArrows = new Vector() );
                  */
            }
            arrows.add( destI );
            //revArrows.add( srcI );
      }


      TreeSet         initials;
      //NondetWordAutom revNfa ;

      protected void initialize( Tree[] subexpr ) {
            this.posMap = new HashMap();
            this.labelAt = new HashMap();


            this.follow = new HashMap();
            this.labels = new HashSet();
            this.recVars = new HashMap();
            this.pos = 0;
            // determine "Sethi-length" of the regexp
            activeBinders = new Vector();
            for( int i = 0; i < subexpr.length; i++ ) {
                  traverse( subexpr[ i ] );
                  assert ( activeBinders.isEmpty() );
            }

            this.initials = new TreeSet();
            initials.add( new Integer( 0 ));

      }

      protected void initializeAutom() {

            finals   = new TreeMap();        // final states
            deltaq   = new HashMap[ pos ];   // delta
            defaultq = new Vector[ pos ];    // default transitions

            for( int j = 0; j < pos; j++ ) {
                  deltaq[ j ] = new HashMap();
                  defaultq[ j ] = new Vector();
            }
      }

      void collectTransitions() {                 // make transitions

            for( int j = 0; j < pos; j++ ) {

                  Integer q = new Integer( j );

                  //System.out.print( "--q="+q );
                  //System.out.println(" labelAt:"+labelAt.get( q ));

                  TreeSet fol = (TreeSet) this.follow.get( q );
                  //assert fol != null;
                  for( Iterator it = fol.iterator(); it.hasNext(); ) {
                        Integer p = (Integer) it.next();
                        //System.out.println( "--  -- p="+p );
                        if( p.intValue() == pos ) {
                              finals.put( q, finalTag );
                        } else {
                              makeTransition( new Integer(j), p,
                                              (Label) labelAt.get( p ));
                        }
                  }
            }

      }

      Integer finalTag;

      public NondetWordAutom automatonFrom( Tree pat, Integer finalTag ) {

            this.finalTag = finalTag;

            //System.out.println( "enter automatonFrom(...)"); // UNIT TEST
            //System.out.println( TextTreePrinter.toString(pat) );
            /*DEBUG = new TextTreePrinter( System.out );
            DEBUG.begin();
            DEBUG.print( pat );
            DEBUG.end();
            */
            //System.out.println( nullableSequence( pat )); // UNIT TEST
            switch( pat ) {
		//case Subsequence( Tree[] subexpr ): // NEW VERSION
		//return automatonFrom( new Tree.Sequence( subexpr ), finalTag ); // NEW VERSION
            case Sequence( Tree[] subexpr ):
                  initialize( subexpr );


                  // (1) compute first

                  //globalFirst = compFirst( subexpr );
                  //System.out.println(globalFirst);

                  // (2) compute follow;
                  ++pos;
                  //Set ignore = compFollow( subexpr );
                  //System.out.println(ignore);
                  //System.exit(0);
                  //assert (ignore.equals( globalFirst ));

                  globalFirst = compFollow( subexpr );

                  //System.out.print("someFirst:");debugPrint(someFirst);

                  // construct the automaton's explicit representation

                  initializeAutom();


                  // defaultqRev = new Vector[ pos ];  // default transitions

                  collectTransitions();


                  //TreeSet initials = new TreeSet();
                  //initials.add( new Integer( 0 ) );

                  NondetWordAutom result =
                        new NondetWordAutom(pos, // = nstates
                                            labels,
                                            initials,
                                            finals,
                                            (Object) deltaq,
                                            (Object) defaultq,
                                            null/*(Object) qbinders*/);

                  /*
                  System.out.println("inBerrySethi");
                  XMLAutomPrinter pr = new XMLAutomPrinter( System.out );
                  pr.begin();
                  pr.print( result );
                  pr.print( revNfa );
                  pr.end();
                  System.out.println("initialsRev = "+initialsRev);
                  System.out.println("outBerrySethi");
                  */
                  //System.exit(0);
                  return result;                  //print();
            }

            throw new ApplicationError("expected a sequence pattern");
      }

      void print1() {
            System.out.println("after sethi-style processing");
            System.out.println("#positions:" + pos);
            System.out.println("posMap:");

            for( Iterator it = this.posMap.keySet().iterator();
                 it.hasNext(); ) {
                  Tree t = (Tree) it.next();
                  switch(t) {
                  case Literal( Object value ):
                        System.out.print( "(" + t.toString() + " -> ");
                        String s2 = ((Integer) posMap.get(t)).toString();
                        System.out.print( s2 +") ");
                  }
            }
            System.out.println("\nfollow: ");
            for( int j = 1; j < pos; j++ ) {
                  TreeSet fol = (TreeSet) this.follow.get(new Integer(j));
                  System.out.print("("+j+" -> "+fol.toString()+") ");
                  //debugPrint( fol );
                  System.out.println();
            }

      }



}
