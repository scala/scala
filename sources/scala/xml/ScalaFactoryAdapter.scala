package scala.xml ;

import scala.xml.javaAdapter.Map ;
import scala.xml.javaAdapter.HashMap ;

abstract class ScalaFactoryAdapter
    extends scala.xml.FactoryAdapter()  {

    def iterToList[ a ]( iter:java.util.Iterator ):List[a] =
        if( !iter.hasNext() )
            Nil
        else
            (iter.next() as a )::iterToList( iter ) ;


    def mapToMap[a,b]( map:java.util.Map ):Map[a,b] = {

         val keys:java.util.Iterator = map.keySet().iterator();
         val res = new HashMap[a,b] ;

         def iterToMap:Unit =
         if( keys.hasNext() ) {
              val key   = keys.next();
              val value = map.get( key ) as b ;
              res.put( key as a , value as b );
              iterToMap
         } else
              () ;

        iterToMap;
        res
    }


    val f: Map[ String, List[Element] => Element ];

    val g: Map[ String, Boolean ] ;

    def   elementContainsText( name:String ):Boolean =
         g.get( name ) ;

    def   createElement(elemName:String,
                        attribs:java.util.Map,
                        children:java.util.Iterator ):scala.Object = {
          val c = f.get( elemName ); // constructor
          val el = c( iterToList[Element]( children ) );
          el.setAttribs( mapToMap[String,String]( attribs ) );
	  el
    }

    def createPCDATA( text:String ):scala.Object  = {
          new PCDATA( text );
    };

} // ScalaFactoryAdapter
