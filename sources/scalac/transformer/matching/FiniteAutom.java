package scalac.transformer.matching ;

import java.util.Iterator ;
import java.util.HashSet ;
import java.util.HashMap ;
import java.util.TreeSet ;
import java.util.TreeMap ;

public abstract class FiniteAutom {

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

      public Object deltaq;

      public Object defaultq; // this gives the default transitions

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
      public Object deltaq( int state ) {
            return ((Object []) deltaq)[ state ];
      }

      /** returns the transitions
       */
      public Object deltaq( Integer state ) {
            return ((Object []) deltaq)[ state.intValue() ];
      }


      /** returns the transitions
       */
      public Object defaultq( int state ) {
            return ((Object []) defaultq)[ state ];
      }

      /** returns the transitions
       */
      public Object defaultq( Integer state ) {
            return ((Object []) defaultq)[ state.intValue() ];
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


}
