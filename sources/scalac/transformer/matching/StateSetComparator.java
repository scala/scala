package scalac.transformer.matching ;

import java.util.* ;

class StateSetComparator implements Comparator {
            // use lexicographic order
            public int compare( Object o1, Object o2 ) {
                  /*
                  System.out.print("lexi" );
                  System.out.print( o1 +" ");
                  System.out.println( o2 );
                  */
                  Iterator it1 = ((TreeSet) o1).iterator();
                  Iterator it2 = ((TreeSet) o2).iterator();
                  while( it1.hasNext() ) {
                        while( it2.hasNext() ) {
                              if( !it1.hasNext() )
                                    return -1;

                              int i1 = ((Integer) it1.next()).intValue();
                              int i2 = ((Integer) it2.next()).intValue();
                              if( i1 < i2 )
                                    return -1;
                              else if ( i1 > i2 )
                                    return 1;
                        }
                        if( it1.hasNext() )
                              return 1;
                  }
                  if( it2.hasNext() )
                        return -1;
                  return 0;
            }
      }
