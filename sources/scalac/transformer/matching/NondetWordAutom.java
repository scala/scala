package scalac.transformer.matching ;

import scalac.ApplicationError ;
import scalac.ast.Tree ;
import scalac.util.Name ;
import Tree.* ;

import java.util.* ;

/** a nondeterministic word automaton.
 *  states are represented (implicitly) as Integer objects.
 */

public class NondetWordAutom  {

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

      public Vector[]  defaultq; // this gives the default transitions

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
      public Vector defaultq( int state ) {
            return defaultq[ state ];
      }

      /** returns the transitions
       */
      public Vector defaultq( Integer state ) {
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


      // inherited from FiniteAutom

      // set of *distinct* objects that may label transitions
      // see Object.hashCode() to see why this works

      //HashSet labels;
      //TreeMap finals;

      TreeSet initials; // ? need this ?
      // ---

      // Object deltaq -->
      // HashMap deltaq[]; // this gives the transitions of q;

      boolean leftTrans;
      boolean rightTrans;

      /** if true, this automaton behaves as a special left transducer.
       *  if a run succeeds, the result is not "true" but the entire
       *  run of the automaton as a sequence of (label,state) - pairs.
       *  used for binding variables.
       */
      public boolean producesRun() {
            return leftTrans;
      }

      public boolean consumesRun() {
            return rightTrans;
      }

      public boolean initial( Integer i ) {
            return initials.contains( i );
      }
      public Vector qbinders[];

      /** returns all states accessible from Qsrc via label.
       *  used by class DetWordAutomaton.
       */
      TreeSet getSide ( TreeSet Qsrc, Object label ) {
            TreeSet Qdest = new TreeSet();
            for( Iterator it = Qsrc.iterator(); it.hasNext(); ) {// state
                  int q = ((Integer) it.next()).intValue();
                  Vector ps = (Vector) deltaq[ q ].get( label );
                  if( ps!=null ) {
		      Qdest.addAll( ps );
                  }
                  Qdest.addAll( defaultq( q ) );
            }
            return Qdest;
      }

      /** returns the transitions
       */
      public Object defaultq( Set P1 ) {
            TreeSet defTarget = new TreeSet();
            for( Iterator p1 = P1.iterator(); p1.hasNext(); ) {
                  int q1 = ((Integer) p1.next()).intValue();
                  //System.out.println("   q1:"+q1+ " defa: "+defaultq( q1 ));
                  defTarget.addAll( defaultq( q1 ) );
            }
            return defTarget;
      }


      /** first match policy: among several final states, the smallest one is
       *   chosen. used by class DetWordAutomaton
       */
      Integer finalTag( Set Q ) {

            int min = Integer.MAX_VALUE ;

            for( Iterator it = Q.iterator(); it.hasNext(); ) {
                  Integer state = (Integer) it.next();
                  Integer tag = (Integer) finals.get( state );
                  if( tag != null ) {
                        if( tag.intValue() < min )
                              min = tag.intValue();
                  }
            }

            if ( min == Integer.MAX_VALUE )
                  throw new ApplicationError( "there should be a final state ");

            return new Integer( min );
      }

      /*
      protected void tmap(int offs, TreeMap t) {
            TreeMap nt = new TreeMap();
            for(Iterator it = t.keySet().iterator(); it.hasNext(); ) {
                  Integer key = (Integer) it.next();
                  Integer newkey = new Integer( key.intValue() + offs );
                  Integer val = (Integer) t.get( key );
                  Integer newval = new Integer( val.intValue() + offs );

                  nt.put( newkey, newval );
            }
            return nt;
      }
*/
      // hashmaps, treemaps
      protected Map mapmap(Map src,
                             int offset,
                             Map dest,
                             boolean mapkeys,
                             boolean mapvals) {
            for(Iterator it = src.keySet().iterator(); it.hasNext(); ) {
                  Object key = it.next();
                  Object val = src.get( key );
                  if( mapkeys ) key = new Integer( ((Integer)key).intValue()
                                                   + offset );
                  if( mapvals ) val = vmap( offset, (Vector) val ) ;
                                      /* new Integer( ((Integer)val).intValue()
                                                   + offset );
                                      */
                  dest.put( key, val );
            }
            return dest;
      }

      protected static Vector vmap(int offs, Vector v ) {
            if( v == null )
                  return null;
            Vector res = new Vector( v.size() );

            for(Iterator it = v.iterator(); it.hasNext(); ) {
                  Integer item = (Integer) it.next();
                  res.add( new Integer( item.intValue() + offs ));
            }
            return res;

      }

      /*
      protected void relocate_defaultq( int offs, Vector   _defaultq[] ) {
            for( int i = 0; i < this.nstates; i++ ) {
                  _defaultq[ i + offset ] = vmap( offset, ((Vector[])defaultq)[ i ]);
            }
      }
      */

      /** copies the values in the fields of this object into the
       *  arguments, possibly adding an offset.
       */
      protected void relocate( int      offset,
                               TreeMap  _finals,
                               HashMap  _deltaq[],
                               Vector   _defaultq[],
                               Vector   _qbinders[] ) {

            mapmap( finals, offset, _finals, true, false);

            for( int i = 0; i < this.nstates; i++ ) {

                  _deltaq  [ i + offset ] = (HashMap) mapmap( ((HashMap[])deltaq)[ i ],
                                                    offset, new HashMap(), false, true);

                  _defaultq[ i + offset ] = vmap( offset, defaultq[ i ] );

            }
            if ((_qbinders != null) &&( qbinders != null ))
                  for( int i = 0; i < this.nstates; i++ ) {
                        //System.out.println("hallo"+qbinders);
                        //System.out.println("qbinders[ i ] :"+qbinders[ i ]);
                        assert _qbinders != null;
                        //System.out.println("_qbinders :"+_qbinders);

                        _qbinders[ i + offset ] = qbinders[ i ];
                  }
      }

      /** if there is more than one initial state, a new initial state
       *  is created, with index 0
       */
      protected void normalize( TreeSet initials, boolean reloc ) {
            //if( initials.size() == 1 )
            //      return;

            HashMap idelta   = new HashMap();
            TreeSet idefault = new TreeSet();

            Integer q0 = new Integer( 0 );

            for( Iterator it = initials.iterator(); it.hasNext(); ) {

                  Integer ostate = (Integer) it.next();

                  Integer finTag = (Integer) finals.get( ostate ) ;
                  if(( finTag != null ) && ( finals.get( q0 )  == null))
                        finals.put( q0, finTag );


                  HashMap tmp = deltaq( ostate );

                  if( reloc )
                        tmp = (HashMap) mapmap( tmp, 1, new HashMap(), false, true );

                  for( Iterator labs = tmp.keySet().iterator(); labs.hasNext(); ) {
                        Label  lab    = (Label) labs.next();
                        Vector itarget = (Vector) idelta.get( lab );
                        if( itarget == null )
                              idelta.put( lab, itarget = new Vector());
                        Vector otarget = (Vector) tmp.get( lab );
                        itarget.addAll( otarget );
                  }
                  //System.out.println( "normalize:defaultq[ "+ostate+" ] "+((Vector[]) defaultq) [ ostate.intValue() ]);
                  if( defaultq( ostate ) != null )
                        idefault.addAll( defaultq( ostate )  );
            }

            if( reloc ) {
                  int m = 1 + this.nstates;
                  TreeMap _finals     = new TreeMap();
                  HashMap _deltaq[]   = new HashMap[ m ];
                  Vector  _defaultq[] = new Vector[ m ];
                  Vector  _qbinders[] = null;

                  if( qbinders != null )
                        _qbinders = new Vector[ m ];

                  relocate( 1, _finals, _deltaq, _defaultq, _qbinders );

                  this.nstates  = m;
                  this.finals   = _finals;
                  this.deltaq   = _deltaq;
                  this.defaultq = _defaultq;
                  this.qbinders = _qbinders;
            }

            this.deltaq  [ 0 ] = idelta;
            //System.out.println("normalize:deltaq[ 0 ]"+ idelta );
            this.defaultq[ 0 ] = new Vector( idefault );

            //System.out.println("normalize:defaultq[ 0 ]"+ idefault );

            this.initials = new TreeSet();
            this.initials.add( q0 );
      }


      /** needed for NondetWordSwitch
       */
      NondetWordAutom() {}

      /** called from Berry-Sethi construction.
       */

      public NondetWordAutom(int nstates,
                             HashSet labels,
                             TreeSet initials,
                             TreeMap finals,
                             HashMap[] deltaq,
                             Vector[]  defaultq,
                             Object qbinders) {
            this.nstates = nstates;
            this.labels = labels;
            this.initials = initials;
            this.finals = finals;
            this.deltaq = deltaq;
            this.defaultq = defaultq;
            this.qbinders = (Vector[])qbinders;
            //print();
            //System.exit(0);
      }



      int offset[]; // only used if constructed from multiple

      protected void collectLabels( NondetWordAutom nfa[] ) {
            this.labels = new HashSet();
            for( int i = 0; i < nfa.length; i++ ) {
                  this.labels.addAll( nfa[ i ].labels );
            }
      }

      protected void collectOffsets( NondetWordAutom nfa[] ) {
            this.offset = new int[ nfa.length + 1 ];
            offset[ 0 ] = 1; // we have a new initial state
            for( int i = 0; i < nfa.length ; i++ )
                  offset[ i + 1 ] = nfa[ i ].nstates + offset[ i ];

      }

      /** collapses several normalized NondetWordAutom objects into one.
       */

      public NondetWordAutom( NondetWordAutom nfa[] ) {


            //this.m
            int m = nfa.length;
            //System.out.println("enter NondetWordSwitch, "
            //                   +"combining " + m + " automata");

            collectLabels( nfa );
            collectOffsets( nfa );

            this.nstates = offset[ nfa.length ]; //m - 1 ] + nfa[ m - 1 ].nstates;


            this.finals = new TreeMap();

            this.qbinders = new Vector[ nstates ];

            // new initial state gets all transitions from all initial states

            this.deltaq        = new HashMap[ nstates ];
            this.defaultq      = new Vector [ nstates ];

            //TreeSet defaultqSet = new TreeSet();
            deltaq[ 0 ] = new HashMap(); // 0 = our new initial state

            TreeSet initials = new TreeSet();


            for( int i = 0; i < m; i++ ) {
                  //System.out.println("i (current NFA):"+i);

                  NondetWordAutom n = nfa[ i ];

                  int offs = offset[ i ];

                  initials.add( new Integer( offs ));

                  n.relocate( offs,
                              this.finals,
                              this.deltaq,
                              this.defaultq,
                              (Vector[])  this.qbinders );
            }

            normalize( initials, false );

      }




      public void print() {

            System.out.print("NFA on labels "+ this.labels);

            if( offset != null ) {
                  System.out.print("offset");
                  for( int k = 0; k < offset.length; k++ ) {
                        if( k > 0)
                              System.out.print(", ");
                        System.out.print(offset[k]);
                  }
            }
            System.out.println();

            System.out.print("max state number :" + (nstates - 1)  );

            System.out.println("initials" + initials);

            System.out.println("finals" + finals);

            for( int i = 0; i < nstates; i++ ) {
                  System.out.print("state: " + i);
                  if( finals.containsKey( new Integer( i )) ){
                        System.out.print("*"); //final
                  }
                  System.out.print("  transitions: {");
                  HashMap arrows = deltaq[ i ];

                  for( Iterator it = arrows.keySet().iterator();
                       it.hasNext(); ) {
                        Object label = it.next();
                        Vector targets = (Vector) arrows.get( label );
                        for( Iterator jt = targets.iterator();
                             jt.hasNext(); ) {
                              Integer p = (Integer) jt.next();
                              System.out.print("("+label+","+p+")");
                        }
                  }

                  System.out.print("} ");
                  System.out.print(" default transitions: "+defaultq( i ));
                  if( qbinders != null )
                        System.out.println(" binders "+qbinders[ i ]);
                  System.out.println();

            }

      }


}
