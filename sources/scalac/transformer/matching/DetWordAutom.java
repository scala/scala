package scalac.transformer.matching ;

import scalac.ast.Tree ;
import Tree.* ;

import java.util.* ;

import scalac.ApplicationError ;

public class DetWordAutom  {

    // BEGIN stuff from FiniteAutom

      //final static Integer FINTAG = new Integer(0);

      /** number of states */
      protected int nstates;

      /** the 'alphabet' */
      protected HashSet labels;

      /** the set of final states, here as a TreeMap */
      protected TreeMap finals;

      /** dfa: HashMap trans: Object -> Integer
       *  nfa: HashMap trans: Object -> Vector [ Integer ]
       *
       *  nfa: Integer  ->(Object -> Vector [ Int ])
       *       [q]     |->( a |-> { q' | (q,a,q') in \deltaright } )
       *
       *  dfa: Integer  ->(Object -> Int)
       *       [q]     |->( a |-> q' | \deltaright(q,a) = q' } )
       */

      public HashMap[] deltaq;

      public Integer[] defaultq; // this gives the default transitions

      //protected HashMap deltaq[];

      // --- accessor methods

      /** returns number of states
       */
      public int nstates() {
            return nstates;
      }

      /** returns the labels
       */
      public HashSet labels() {
            return labels;
      }

      /** returns the transitions
       */
      public HashMap deltaq( int state ) {
            return deltaq[ state ];
      }

      /** returns the transitions
       */
      public HashMap deltaq( Integer state ) {
            return deltaq[ state.intValue() ];
      }


      /** returns the transitions
       */
      public Integer defaultq( int state ) {
            return defaultq[ state ];
      }

      /** returns the transitions
       */
      public Integer defaultq( Integer state ) {
            return defaultq[ state.intValue() ];
      }


      /** returns true if the state is final
       */
      public boolean isFinal( int state ) {
            return ((finals != null)
                    && (finals.get( new Integer( state )) != null));
      }

      /** returns true if the state is final
       */
      public boolean isFinal( Integer state ) {
            return ((finals != null) && finals.containsKey( state ));
      }

      /** returns true if the state is final
       */
      public Integer finalTag( Integer state ) {
            return (Integer) finals.get( state );
      }


      public Integer finalTag( int state ) {
            return (Integer) finals.get( new Integer (state ));
      }

      /** returns true if the set of states contains at least one final state
       */
      boolean containsFinal( TreeSet Q ) {
            for( Iterator it = Q.iterator(); it.hasNext(); ) {
                  if( isFinal( (Integer) it.next()))
                        return true;
            }
            return false;
      }


      /** returns true if there are no finite states
       */
      public boolean isEmpty() {
            return finals.isEmpty();
      }

    // END stuff from FiniteAutom

      static final int FIRST = 0;
      static final int LAST  = FIRST + 1;

    //static final int WHICH_LONGEST_MATCH = FIRST ;
      static final int WHICH_LONGEST_MATCH = LAST ;

      // inherited from FiniteAutom:

      // int nstates;   // number of states
      // HashSet labels;// the alphabet
      // TreeMap finals;

      // HashMap deltaq[];
      //Integer defaultq[];


      // TEMPORARY VAR used only during determinization and debug printing
      // Q -> (Label -> Q )
    HashMap delta;
    // Q -> Integer;
    HashMap indexMap;

      // Integer -> Q
      HashMap invIndexMap;

      // only not null if this is a right-transducer
      public Vector qbinders[];

      final static Integer NODEFAULT = new Integer( -1 );

      public boolean isSink( int i ) {
	  return  ( deltaq[ i ].keySet().isEmpty()
		    && (defaultq != null )
		    && (defaultq( i ).intValue() == i) );
      }

      public boolean hasDefault( int i ) {
            return defaultq( i ) != NODEFAULT;
      }

      void determinize( NondetWordAutom nfa ) {
            //System.out.println("DetWordAutom:determinize");
            //System.out.println("nfa:");nfa.print();
            TreeSet states;// temp: Set[Set[Integer]]
            HashMap deftrans; // Set[Integer] -> Int

            HashMap trans; // always points to a mapping ( Label -> Q )
            int ix = 0;    // state index

            this.labels = nfa.labels;
            ////System.out.println("Labels: "+labels);
            this.delta = new HashMap();
            //this.dead = -1;

            states = new TreeSet( new StateSetComparator() );
            deftrans = new HashMap();
            // temporarily: Map[Set[Integer]] later: Map[Integer]
            this.finals = new TreeMap( new StateSetComparator() );
            this.invIndexMap = new HashMap();
            this.indexMap = new HashMap();

            // new initial state (singleton set { q0 } by construction)

            TreeSet q0 = new TreeSet();
            q0.addAll( nfa.initials ); /*new Integer( 0 )); */
            states.add( q0 );

            TreeSet empty = new TreeSet();
            deftrans.put( q0, empty );
            states.add( empty );
            deftrans.put( empty, empty );

            Stack rest = new Stack();
            if( nfa.isFinal( 0 ) )
                  this.finals.put( q0, nfa.finalTag( 0 ) );


            rest.push( empty );
            rest.push( q0 );
            while( !rest.empty() ) {
                  TreeSet P1 = (TreeSet) rest.pop();

                  //System.out.println("states:"+ states);
                  //System.out.println("P1:"+ P1);

                  invIndexMap.put( new Integer( ix ), P1 );
                  indexMap.put( P1, new Integer( ix++ ));
                  delta.put( P1, trans = new HashMap());

                  // labelled transitions

                  for( Iterator it = labels.iterator(); it.hasNext(); ) {
                        Object label = it.next();
                        ////System.out.print( "Label: " + label +" ");
                        // Qdest will contain all states reachable via `label'
                        // from some nfa state in P1;
                        TreeSet Qdest = nfa.getSide( P1, label );
                        //System.out.println("Qdest:"+Qdest);
                        if( !states.contains( Qdest ) ) {
                              states.add( Qdest );
                              ////System.out.print(" (added)" );
                              rest.push( Qdest );
                              ////System.out.print(" (pushed)");

                              if( nfa.containsFinal( Qdest ) )
                                    this.finals.put( Qdest, nfa.finalTag( Qdest ));
                              ////System.out.print(" (added final)");

                        }
                        ////System.out.println(".Qdest");

                        trans.put( label, Qdest );
                        // //System.out.println( "Qdest: " + Qdest);

                  }

                  // default transitions

                  TreeSet defTarget = (TreeSet) nfa.defaultq( P1 );
                  //System.out.println("defTarget:"+defTarget);
                  deftrans.put( P1, defTarget );

                  if( !states.contains( defTarget ) ) {
                        states.add( defTarget );
                        rest.push( defTarget );
                        if( nfa.containsFinal( defTarget ) )
                              this.finals.put( defTarget, nfa.finalTag( defTarget ));
                  }
            }

            // <DEBUG>
            // printBefore( states, deftrans );

            // </DEBUG> do not call printBefore after this point
            // //System.out.println("indexMap: "+indexMap);

            this.nstates = states.size();
            deltaq = new HashMap[ nstates ];
            defaultq = new Integer[ nstates ];

            // we replace Set[Set[Integer]] by its index and clean up

            for( Iterator it = states.iterator(); it.hasNext(); ) {
                  TreeSet state   = (TreeSet) it.next();
                  Integer state_x = (Integer) indexMap.get( state );

                  TreeSet defTarget  = (TreeSet) deftrans.get( state );
                  Integer defTarget_x;
                  if( defTarget != null ) {
                        defTarget_x = (Integer) indexMap.get( defTarget );
                        ////System.out.println("deftarget" + defTarget);
                  } else
                        defTarget_x = NODEFAULT;

                  ////System.out.print(state.toString() + " --> " + state_x);
                  //System.out.println(" deftarget " + defTarget + " --> "+defTarget_x);

                  trans = (HashMap) delta.get( state );
                  HashMap newTrans = new HashMap();
                  for( Iterator labs = labels.iterator(); labs.hasNext() ;) {
                        Object label = labs.next();
                        TreeSet target   = (TreeSet) trans.get( label );
                        Integer target_x;
                        if( target != null ) {
                              // //System.out.println("target :"+target);
                              target_x = (Integer) indexMap.get( target );

                              if( target_x.intValue() != defTarget_x.intValue() ) {
                                    // replace target by target_x
                                    // (use type-unawareness)
                                    newTrans.put( label, target_x );
                              }
                              trans.remove( label );
                        }

                  }
                  deltaq[ state_x.intValue() ] = newTrans;
                  defaultq[ state_x.intValue() ] = defTarget_x;

                  delta.remove( state );
                  deftrans.remove( state );

            }

            TreeMap oldfin = finals;
            this.finals = new TreeMap();
            for( Iterator it = oldfin.keySet().iterator(); it.hasNext(); ) {
                  TreeSet state = (TreeSet) it.next();
                  Integer state_x = (Integer) indexMap.get( state );
                  this.finals.put( state_x, oldfin.get( state ) );// conserve tags
            }

            // clean up, delete temporary stuff
            /*
              // we cannot clean up, indexmap is needed later
            for( Iterator it = states.iterator(); it.hasNext(); ) {
                  ((TreeSet) it.next()).clear();
            }
            */
            states.clear();

            //minimize();
      }

      public DetWordAutom() {}

      public boolean isDead( int state ) {
            return state == nstates - 1; // by construction
      }

      public boolean isDead( Integer state ) {
            return state.intValue() == nstates - 1; // by construction
      }

      /** determinization -- standard algorithm considering only
       *                    reachable states
       */
      public DetWordAutom( NondetWordAutom nfa ) {
            determinize( nfa );
      }

      /** for a set of nfa states (that must exist), returns its transitions
       */
      HashMap deltaq( TreeSet nset ) {
            return deltaq( (Integer) indexMap.get( nset ) );
      }


      /** for a set of nfa states (that must exist), returns its transitions
       */
      Integer defaultq( TreeSet nset ) {
            return defaultq( (Integer) indexMap.get( nset ) );
      }

      /** returns target of the transition from state i with label label.
       *  null if no such transition exists.
       */
      Integer delta( int i, Label label ) {
            Integer target;
            switch( label ) {
            case DefaultLabel:
                  if( !hasDefault( i ) )
                        return null;
                  return (Integer) defaultq( i ) ;
            case SimpleLabel( _ ):
            case TreeLabel( _ ):
                  return (Integer) deltaq[ i ].get( label ) ;
                  /*case Pair( Integer state, Label lab ):
                  return state;
                  */
            default:
                  throw new ApplicationError("whut's this: label="+label+", class "+label.getClass());
            }
      }

      Integer delta( Integer i, Label label ) {
            return delta( i.intValue(), label );
      }

      /** should maybe in nfa, not here
       */
      protected static Integer smallestFinal( NondetWordAutom nfa,
                                              TreeSet states ) {

            int min = Integer.MAX_VALUE ;
            for( Iterator it = states.iterator(); it.hasNext(); ) {
                  Integer state = (Integer) it.next();
                  if( nfa.isFinal( state ) && (state.intValue() < min ))
                        min = state.intValue();
            }
            if( min == Integer.MAX_VALUE )
                  throw new ApplicationError("I expected a final set of states");
            return new Integer( min );

      }

      protected Vector allSetsThatContain( Integer ndstate ) {
            Vector v = new Vector();
            for( Iterator it = indexMap.keySet().iterator(); it.hasNext(); ) {
                  TreeSet ndstateSet = (TreeSet) it.next();
                  if( ndstateSet.contains( ndstate ))
                        v.add( ndstateSet );
            }
            return v;
      }


      protected void filterItOutQuoi( DetWordAutom dLeft,
                                      Cartesian.Npair npTarget,
                                      Label.Pair lab,
                                      TreeMap nsrc ) {
            Label theLabel  = lab.lab;
            Integer ntarget = lab.state;

            // e.g.[2,(3),4] --> 7
            Integer dstate = (Integer) dLeft.indexMap.get( npTarget.nset );

            // eg. 3 -> [3] [2,3]
            Vector targets = dLeft.allSetsThatContain( ntarget );

            ////System.out.println( targets+", of these " ) ;

            // filter out those source states which arrive here...

            for( Iterator su = targets.iterator(); su.hasNext(); ) {
                  TreeSet nset   = (TreeSet) su.next();

                  HashMap ddelta = dLeft.deltaq( nset );

                  // ...  at THIS dstate
                  if( (Integer) ddelta.get( theLabel ) == dstate ) {

                        Cartesian.Npair np1 = new Cartesian.Npair( ntarget, nset );

                        ////System.out.print( np1.toString( dLeft.indexMap ));

                        if( WHICH_LONGEST_MATCH == FIRST )
                              addTransitionFLM( nsrc, np1 );
                        else
                              addTransitionLLM( nsrc, np1 );
                  }

            }
      }

      /** all default transitions from sets that contain nq to npTarget
       */
      protected void filterItOutQuoiDefault( DetWordAutom dLeft,
                                             Cartesian.Npair npTarget,
                                             Integer nq,
                                             TreeMap nsrc ) {


            ////System.out.println( "npTarget = " + npTarget ) ;

            Vector allSources = dLeft.allSetsThatContain( npTarget.nstate );

            for( Iterator it = allSources.iterator(); it.hasNext(); ) {

                  // e.g.[2,(3),4] --> 7
                  //Integer dstate = (Integer) dLeft.indexMap.get( npTarget.nset );

                  Integer dstate = (Integer) dLeft.indexMap.get( it.next() );

                  //System.out.println( "dstate = " + dstate ) ;

                  assert dstate != null;

                  // eg. 3 -> [3] [2,3]
                  Vector targets = dLeft.allSetsThatContain( nq );

                  //System.out.println( "targets: " + targets ) ;

                  // filter out those source states which arrive here...

                  for( Iterator su = targets.iterator(); su.hasNext(); ) {
                        TreeSet nset   = (TreeSet) su.next();

                        Integer ddef = dLeft.defaultq( nset );

                        //System.out.println( "ddef ="+ddef );

                        // ...  at THIS dstate
                        if( ddef == dstate ) {

                              Cartesian.Npair np1 = new Cartesian.Npair( nq, nset );

                              // print target
                              //System.out.print( np1.toString( dLeft.indexMap ));

                              if( WHICH_LONGEST_MATCH == FIRST )
                                    addTransitionFLM( nsrc, np1 );
                              else
                                    addTransitionLLM( nsrc, np1 );

                        }

                  }
            }
      }

      /** this implements the first longest match policy
       */
      protected static void addTransitionFLM( TreeMap nsrc, Cartesian.Npair np ) {
            Cartesian.Npair np2 = (Cartesian.Npair) nsrc.get( np.nset );

            // (policy) first longest match
            if(( np2 == null )
               ||( np2.nstate.intValue() > np.nstate.intValue())) {
                  nsrc.put( np.nset, np  );
            }

      }

      /** this implements the last longest match policy (!)
       */
      protected static void addTransitionLLM( TreeMap nsrc, Cartesian.Npair np ) {
            Cartesian.Npair np2 = (Cartesian.Npair) nsrc.get( np.nset );

            // (policy) first longest match
            if(( np2 == null )
               ||( np2.nstate.intValue() < np.nstate.intValue())) {
                  nsrc.put( np.nset, np  );
            }

      }


      /** build a deterministic right to left transducer from the args
       */
      public DetWordAutom( NondetWordAutom right,
                           NondetWordAutom left,
                           DetWordAutom    dLeft ) {

            /* System.out.println("DetWordAutom.<init>(nfa,nfa,dfa)");
               System.out.println("nfa-left:");left.print();
               System.out.println("nfa-right:");right.print();
               System.out.println("dLeft:"+dLeft.print());
               System.out.println("dLeft.finals"+dLeft.finals);
            */
            this.indexMap = dLeft.indexMap;
            this.invIndexMap = dLeft.invIndexMap;
            // fix indexMap
            /* // unnecessary
            TreeSet q0 = new TreeSet();
            q0.add( new Integer( 0 ));
            indexMap.put( q0, new Integer( 0 ));
            //System.out.println("check out the indexMap!" + indexMap);
            */

            TreeSet visited_n = new TreeSet( new NpairComparator() );
            Stack rest    = new Stack();

            // right is "nearly deterministic"
            // we can follow reverse traces paths by using dLeft.indexMap

            // start with right.initials, left.final, dLeft.final
            for( Iterator it = dLeft.finals.keySet().iterator(); it.hasNext(); ) {
                  Integer fstate  = (Integer) it.next();
                  TreeSet nfstate = (TreeSet) invIndexMap.get( fstate );
                  //System.out.print( "final state:"+fstate);
                  //System.out.print( " correspond to set of states:"+ nfstate );

                  Integer min_ndstate = smallestFinal( left, nfstate );

                  Cartesian.Npair npair = new Cartesian.Npair( min_ndstate, nfstate );

                  //System.out.println( "  smallest final of these: "+ min_ndstate );


                  //System.out.println( "push final nfa state "+npair.toString( dLeft.indexMap ));

                  if( !visited_n.contains( npair )) {
                        visited_n.add( npair );
                        rest.push( npair );
                  }
            }

            HashMap ratLab     = new HashMap(); // maps nset to label,HashMap
            HashMap ratDelta   = new HashMap(); // maps nset to Vector[ NP ]targets

            HashMap ratDefault = new HashMap(); // maps nset to NP (one target)

            int ix = 1;
            Stack ix_initial = (Stack) rest.clone();
            TreeSet ix_final = new TreeSet( new NpairComparator() );;

            TreeMap newIndexMap = new TreeMap( new NpairComparator() );

            while( !rest.isEmpty() ) {

                  Cartesian.Npair npair = (Cartesian.Npair) rest.pop();
                  newIndexMap.put( npair, new Integer(ix));

                  ratDelta.put( npair, new Vector() );

                  if( npair.nset.contains( new Integer( 0 )) ) {
                        ix_final.add( npair );
                  }
                  ix++;

                  //System.out.println(" popped "+npair.toString( dLeft.indexMap ));

                  ////System.out.print(" binders: ");
                  ////System.out.print( right.qbinders[ npair.nstate.intValue() ] );

                  HashMap delta = right.deltaq( npair.nstate );

                  ////System.out.print(" we could have arrived : ");
                  //search the delta for target invIndexMap

                  HashMap labelToNset = new HashMap();
                  HashMap labelToFrom = new HashMap();

                  // maps nsets to the active nstates
                  TreeMap nsrc = new TreeMap( new StateSetComparator() );

                  // berry-sethi construction assures that
                  //   there is only one label for outgoing transitions
                  Label theLabel = null;

                  // collect all transition possible in the DFA

                  for( Iterator it = delta.keySet().iterator(); it.hasNext(); ) {

                        Label.Pair   lab = (Label.Pair) it.next();

                        // lab.state is the target in the NFA

                        if( theLabel == null ) {
                              ratLab.put( npair, lab.lab );
                              ////System.out.print(" with \""+lab.lab+"\" ");
                        }
                        theLabel = lab.lab ;

                        ////System.out.print("\nfrom n" + lab.state +"  ... ");

                        // these are too many, filter out those that exist in DFA

                        filterItOutQuoi( dLeft, npair, lab, nsrc );

                  }


                  ////System.out.println( "---" );

                  ////System.out.println("all sources: ");

                  // !!  first longest match

                  for( Iterator ut = nsrc.keySet().iterator(); ut.hasNext(); ) {
                        TreeSet nset  = (TreeSet) ut.next();

                        Cartesian.Npair np2 = (Cartesian.Npair) nsrc.get( nset );

                        assert( np2 != null );
                        ////System.out.println("target: n"+npair.nstate+" via: "+theLabel+" from "+ np2.toString( dLeft.indexMap ));// nset:"+nset+ " namely state n"+ dest);

                        Vector v = (Vector) ratDelta.get( npair );

                        v.add( np2 );

                        if( !visited_n.contains( np2 ) ) {

                              visited_n.add( np2 );
                              rest.push( np2 );
                        }

                  }

                  //System.out.println("default sources: ");

                  // maps nsets to the active nstates
                  nsrc = new TreeMap( new StateSetComparator() );

                  // now for all default transitions that arrive at this nfa state
                  Vector defqs = right.defaultq( npair.nstate );
                  for( Iterator it = defqs.iterator(); it.hasNext(); ) {
                        Integer nq = (Integer) it.next();
                        //System.out.println("checking nq="+nq);
                        filterItOutQuoiDefault( dLeft, npair, nq, nsrc );
                        //System.out.println( "nsrc after "+nq+" is "+nsrc );
                  }

                  //System.out.println( "defqs :"+defqs );
                  //System.out.println( "nsrc :"+nsrc );

                  for( Iterator ut = nsrc.keySet().iterator(); ut.hasNext(); ) {

                        Cartesian.Npair np2 = (Cartesian.Npair) nsrc.get( ut.next() );

                        Vector v = (Vector) ratDefault.get( npair );
                        if( v == null )
                              ratDefault.put( npair, v = new Vector() );
                        v.add( np2 );

                        if( !visited_n.contains( np2 ) ) {

                              visited_n.add( np2 );
                              rest.push( np2 );
                        }

                  }

                  ////System.out.println("zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz");

            }

            // Renumbering

            ////System.out.println( "output: a dfa with "+ix+"states");

	    // FIX: empty regular expression (as in "List()") is valid
            //assert ( !ix_final.isEmpty() ) : "no final states found";

            ////System.out.println( "final state:"+ix_final);

            //System.out.println( "indexMap: " +indexMap);
            //System.out.println( "newIndexMap: " +newIndexMap);
            this.finals = new TreeMap();
            this.nstates = ix;
            HashMap dratDelta[] = new HashMap[ ix ];
            qbinders  = new Vector[ ix ];
            labels = new HashSet();
            for( Iterator it = ratDelta.keySet().iterator(); it.hasNext(); ) {
                  Cartesian.Npair np = (Cartesian.Npair) it.next();

                  //System.out.print( "\nstate: "+np);
                  TreeSet ndset = np.nset;
                  Integer dstate = (Integer) newIndexMap.get( np );
                  assert dstate != null : "no dstate for "+np.toString(dLeft.indexMap);

                  //System.out.print(" binders:");

                  qbinders[ dstate.intValue() ] = left.qbinders[ np.nstate.intValue() ];

                  //System.out.print( qbinders[dstate.intValue() ]);

                  //System.out.println(" transitions:");
                  if( ix_final.contains( np ) ) {
                        Integer fin_ix = (Integer) newIndexMap.get( np );
                        finals.put( fin_ix, new Integer( 0 ));
                  }

                  Label   lab   = (Label)  ratLab.get( np );
                  Vector  v     = (Vector) ratDelta.get( np );

                  HashMap ddelta = new HashMap();

                  // v might be null if there are only default transitions
                  if( v != null )
                        for( Iterator it2 = v.iterator(); it2.hasNext() ; ) {

                              Cartesian.Npair np2= (Cartesian.Npair) it2.next();
                              //System.out.print( "("+lab+","+np2+") " );
                              Integer ddestR = (Integer) newIndexMap.get( np2 );
                              Integer ddest = (Integer) indexMap.get( np2.nset );
                              assert ddest != null :
                                    "no ddest for "
                                    +np2.toString(dLeft.indexMap);

                              Label.Pair newLab = new Label.Pair(ddest, lab);
                              ddelta.put( newLab, ddestR );
                              labels.add( newLab );

                        }
                  dratDelta[ dstate.intValue() ] = ddelta;

            }

            for( Iterator it = ratDefault.keySet().iterator(); it.hasNext(); ) {
                  Cartesian.Npair np  = (Cartesian.Npair) it.next();
                  Integer         dstate = (Integer) newIndexMap.get( np );

                  //System.out.print("\nstate: "+np+" default trans: ");

                  Vector v = (Vector) ratDefault.get( np );
                  for( Iterator ut = v.iterator(); ut.hasNext(); ) {
                        Cartesian.Npair np2  = (Cartesian.Npair) ut.next();
                        Integer targetL      = (Integer) indexMap.get( np2.nset );
                        Integer targetR      = (Integer) newIndexMap.get( np2 );

                        Label defLab = new Label.Pair( targetL,
                                                       Label.DefaultLabel );

                        labels.add( defLab );
                        //System.out.print( "("+defLab+","+np2+") " );

                        HashMap d = dratDelta[ dstate.intValue() ];
                        if( d == null )
                              dratDelta[ dstate.intValue() ] = d = new HashMap();

                        d.put( defLab, targetR );
                  }
            }

            deltaq = dratDelta;

            HashMap hmap = new HashMap();

            // final states of left are initial states of right
            // problem: still need to choose the one

            while( !ix_initial.isEmpty() ) {
                  Cartesian.Npair np = (Cartesian.Npair) ix_initial.pop();

                  Integer i          = (Integer) newIndexMap.get( np ); //R-state
                  Integer dtarget    = (Integer) indexMap.get( np.nset );// left-d-state

                  hmap.put( dtarget, i );
            }
            deltaq[ 0 ]  = hmap; // careful, this maps Int to Int

            qbinders[ 0 ] = new Vector();
            //((Vector[])defaultq)[ 0 ] = new Vector(); is null
            printBeforeRAT( dratDelta );

      }

      void printBeforeRAT1( String str ) {
            StringBuffer tmp = new StringBuffer( str );
            for( int j = tmp.length(); j < 20; j++ ) {
                  tmp.append(" ");
            }
            //System.out.print( tmp.toString() );
      }

      void printBeforeRAT( HashMap dratDelta[] ) {
            //System.out.println();
            printBeforeRAT1( "dratDelta" );
            printBeforeRAT1( "[index]" );
            //System.out.println();

            for( int i = 0; i < dratDelta.length; i++ ) {
                  if( isFinal( i ))
                        printBeforeRAT1( "*"+i );
                  else
                        printBeforeRAT1( " "+i );

                  //System.out.println( dratDelta[ i ] );
            }
      }

      /** you may only call this before the set[set[...]] representation
       *  gets flattened.
       */
      public void printBefore( TreeSet states, HashMap deftrans ) {
            HashMap trans;
            //System.out.println( states );
            for( Iterator it = states.iterator(); it.hasNext(); ) {
                  TreeSet state = (TreeSet) it.next();
                  //System.out.print("state:"+state.toString()+" transitions ");
                  trans = (HashMap) delta.get( state );
                  for( Iterator labs = labels.iterator(); labs.hasNext() ;) {
                        Object label = labs.next();
                        TreeSet target = (TreeSet) trans.get( label );
                        //System.out.print( "  (" + label.toString()
                        //            + "," + target.toString()+")");
                  }
                  //System.out.print("default trans"+deftrans.get( state ));
                  //System.out.println();
            }
            //System.out.println("final states:" + finals );
      }


      /*
      public void minimize() { // TO DO
            //System.out.println("minimization");
            boolean mark[][] = new boolean[nstates][];
            for( int i = 0; i < nstates; i++ ) {
                  mark[i] = new boolean[nstates - i];
                  for( int j = 0; j < (nstates - i); j++ )
                        mark[i][j] = false;
            }
            debugPrint( mark );
      }

      protected void debugPrint( boolean mark[][] ) {
            for( int i = 0; i < nstates; i++ ) {
                  //System.out.print("[");
                  for( int j = 0; j < nstates - i; j++ ) {
                        //System.out.print(" "+mark[i][j]);
                        if( mark[i][j] )
                              //System.out.print(" ");
                  }
                  //System.out.println(" ]");
            }
      }

      */

      /*

      public void createDeadState() {
            assert dead == -1;
            this.dead = this.nstates++;
            Integer deadI = new Integer( dead );

            HashMap odelta[] = ((HashMap[])deltaq);
            deltaq = new HashMap[ this.nstates ];
            System.arraycopy(odelta, 0, ((HashMap[])deltaq), 0, odelta.length);
            HashMap trans = new HashMap();
            ((HashMap[])deltaq)[ this.dead ] = trans;
            for( Iterator labs = labels.iterator(); labs.hasNext(); ) {
                  trans.put( labels, deadI );
            }
            //System.out.println("createDeadState, new dead state:"+dead);
      }



      // adjusts the alphabet of this automaton

      public void addLabels( HashSet labels ) {

            for(Iterator it = labels.iterator(); it.hasNext(); ) {
                  Object label = it.next();
                  if( this.labels.add( label )) { // new
                        // adjust all transitions

                        if( this.dead == -1 )
                              createDeadState();

                        Integer deadI = new Integer( this.dead );

                        for( int i = 0; i < this.nstates; i++ ) {
                              ((HashMap[])deltaq)[ i ].put( label, deadI );
                        }
                  }
            }
      }
      */

      // wishlist for jaco: why does Cartesian have to be static ?
      // if not, error "inner classes must not have static members"

      /** cartesian
       */

      static class Cartesian {
            /** Int x TreeSet[ Int ]
             */
            case Npair(Integer nstate, TreeSet nset);

            public boolean equals( Object that ) {
                  if( !(that instanceof Cartesian ))
                        return false;
                  switch( this ) {
                  case Npair( Integer nstate, TreeSet nset ):
                        switch((Cartesian) that) {
                        case Npair( Integer _nstate, TreeSet _nset ):
                              return ((nstate == _nstate)
                                      &&( nset == _nset ));
                        }
                  }
                  return false;
            }

            public String toString() {
                  switch( this ) {
                  case Npair( Integer nstate, TreeSet nset ):
                        //Integer dstate = (Integer) indexMap.get( nset );
                        return "<n"+nstate.toString()+" in "+nset /*+" = d"+dstate*/+">";
                  }
                  return null;
            }

            public String toString( HashMap indexMap ) {
                  //assert indexMap != null;
                  switch( this ) {
                  case Npair( Integer nstate, TreeSet nset ):
                        assert nstate!=null;
                        Integer dstate = (Integer) indexMap.get( nset );
                        return "<n"+nstate.toString()+" in "+nset +" = d"+dstate +">";
                  }
                  return null;
            }


      }

      static class NpairComparator extends StateSetComparator {
            public int compare( Object o1, Object o2 ) {
                  if(( o1 instanceof Cartesian.Npair )&&
                     ( o2 instanceof Cartesian.Npair ))
                        switch((Cartesian) o1) {
                        case Npair( Integer nstate, TreeSet nset ):
                              switch( (Cartesian) o2 ) {
                              case Npair( Integer _nstate, TreeSet _nset ):
                                    int res = nstate.compareTo( _nstate );

                                    ////System.out.println("nstate"+nstate+" <> _nstate "+ _nstate+" res"+res);
                                    if( res != 0 )
                                          return res;
                                    else
                                          return super.compare( nset, _nset );
                              }
                        }
                  throw new ApplicationError( "illegal arg. to compare. "
                                              +o1.getClass()+" "+o2.getClass());
            }
      }

}
