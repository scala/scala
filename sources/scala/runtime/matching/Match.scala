package scala.runtime.matching ;

import scala.collection.Map ;
import scala.collection.immutable ;
import scala.xml.Node ;

object Match {

  def res2string(res:immutable.Map[ Int, PathContext ]):String = {
    val sb = new StringBuffer();
    sb.append( "binding {\n" );
    for( val vble <- res.keys ) {
      val pc = res.get( vble ).get;
      sb.append( "  " );
      sb.append( vble );
      sb.append( " <- " );
      sb.append( pc.address.reverse );
      sb.append( '\n' );
    }
    sb.append( "}\n" );
    sb.toString();
  }

  /* pm: remaining pebbles
  */
  def printSol( vble:Int,
                b2:PathContext,
                vctx:immutable.Set[PathContext],
                pm:immutable.Map[ Int, immutable.Set[PathContext] ],
                result:immutable.Map[ Int, PathContext] ):unit = {
    Console.println("printSol("+vble+","+vctx+","+pm+","+result+")");
    for( val b <- vctx.elements; {if( b2!=null ) b.isCompatible( b2 ) else true}) {
      val nres = result.update( vble, b );
      if( pm.isEmpty ) {
        Console.println( nres );
        Console.println( res2string(nres) );
      } else for( val nextVar <- pm.keys;
                 val nextCtx <- pm.get( nextVar )) {
                   printSol( nextVar, b, nextCtx, pm - nextVar, nres )
                 }
    }
  }


  def decode( pebblesMap:immutable.Map[ Int, immutable.Set[PathContext] ] ) = {

    val it = pebblesMap.keys;
    if( it.hasNext ) {
      val first = it.next ;
      val fctx = pebblesMap.get( first ).get;
      printSol( first,
               null,
               fctx,
               pebblesMap - first,
               new immutable.TreeMap[Int,PathContext] )
    }
  }

}


class Match( k:Int,it:Iterator[Seq[scala.xml.Node]] ) {

  override def equals( o:Any ) = o match {
    case that:Match =>
    ( this.index == that.index )&&( this.iter.toList == that.iter.toList )
    case _ => false;
  }

  val index = k;
  val iter = it;

  /** this method is destructive
  **/
  def toString_DEBUG( vx2str:Int => String ) = {
    if( !iter.hasNext ) {
      "<empty binding>"
    } else {
      val sb = new StringBuffer("<match> with binding");
      var vx = 0;
      while( iter.hasNext ) {
        sb.append( vx2str( vx ) + " <- " + iter.next.toString + "\n" );
      }
      sb.toString();
    }
  }

  /** this method is destructive
  **/
  def same( that:Match ) = {
    ( this.index == that.index )&&( this.iter.toList == that.iter.toList )
  }

  /** this method is destructive
  **/
  override def toString() = {
    val sb = new StringBuffer("Match("+index);
    while( iter.hasNext ) {
      sb.append( "," );
      sb.append( iter.next.toString() );
    }
    sb.append(")");
    sb.toString();
  }
}
