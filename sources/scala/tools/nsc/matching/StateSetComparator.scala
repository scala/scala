package scala.tools.nsc.matching ;

import java.util.{Comparator, TreeSet} ;

class StateSetComparator extends Comparator {
  // use lexicographic order
  def compare(o1: Any , o2: Any ): Int = {
    /*
     System.out.print("lexi" );
     System.out.print( o1 +" ");
     System.out.println( o2 );
     */
    val it1 = o1.asInstanceOf[TreeSet].iterator();
    val it2 = o2.asInstanceOf[TreeSet].iterator();
    while( it1.hasNext() ) {
      while( it2.hasNext() ) {
        if( !it1.hasNext() )
          return -1;

        val i1 = it1.next().asInstanceOf[Integer].intValue();
        val i2 = it2.next().asInstanceOf[Integer].intValue();
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
