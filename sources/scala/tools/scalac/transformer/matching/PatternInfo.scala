
import scalac.ast._ ;
import scalac.util.Names ;

package scala.tools.scalac.transformer.matching {

  /** some properties of pattern */
  object PatternInfo {


    /** returns width */
    private def getLowerBoundWidth( it:Iterator[Tree] ):Int = {
      if( !it.hasNext ) 0 else {
        var wid = minimalWidth( it.next );
        for( val h <- it ) {
          val hw = minimalWidth( h );
          if( hw < wid ) {
            wid = hw
          }
        }
        wid
      }
    }

    /** @todo translate this from ems pattern
    */
    def minimalWidth( pat:Tree ):Int = pat match {
      case Tree$Alternative( choices ) => 1 // bogus

      // WildcardTree pattern _
      case Tree$Ident( Names.PATTERN_WILDCARD ) => 1

      // Node pattern t ( ps )    // works also for WildcardNode pattern
      case Tree$Apply( _, _  ) => 1

      // p | ... | p
      case Tree$Alternative( ps ) =>
        getLowerBoundWidth( Iterator.fromArray( ps ) );

      // p1...pN
      case Tree$Sequence( ps ) =>
        Iterator.fromArray( ps ).foldLeft (0) {
          (s:int,x:Tree) => s + minimalWidth( x )
        }

      case Tree$Bind( n, p ) => 1

      case _                    => 0

    }
  }

}
