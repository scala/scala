package scala.xml;

/** Generic.load( <fileName> ) will load the xml document from file and
 *  create a tree with scala.Labelled, PCDATA and scala.Symbol objects.
 *  Text can appear within PCDATA at the leaves.
 */

object Generic {

  def load( filename:String ):Labelled = {
    val b = new GenericFactoryAdapter().loadXML( filename );
    b.asInstanceOf[Labelled]
  };

  class GenericFactoryAdapter extends dtd2scala.FactoryAdapter()  {

    import scala.xml.javaAdapter.Map ;
    import scala.xml.javaAdapter.HashMap ;

    def iterToList[ a ]( iter:java.util.Iterator ):List[a] =
        if( !iter.hasNext() )
            Nil
        else
            (iter.next().asInstanceOf[a])::iterToList( iter ) ;

    def   elementContainsText( name:java.lang.String ):boolean = true;

    // default behaviour is hash-consing
    val cache = new HashMap();

    def   createElement( elemName:String,
                         attribs :java.util.Map, // ignore attributes.
                         children:java.util.Iterator ):scala.Object = {

          val el = Labelled( Symbol( elemName), iterToList[ Any ]( children ));
	  val el_cache = cache.get( el.asInstanceOf[scala.All]).asInstanceOf[scala.Object];
	  if ( el_cache != null ) {
	    System.err.println("[using cached elem!]");
	    el_cache
	  } else {
	    cache.put( el.asInstanceOf[scala.All], el.asInstanceOf[scala.All] );
	    el
	  }


    }

    def createPCDATA( text:String ):scala.Object  = {
          new PCDATA( text );
    };

  } // GenericFactoryAdapter

}
