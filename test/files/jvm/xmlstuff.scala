import java.io.StringReader;
import org.xml.sax.InputSource;
import scala.xml.nobinding.XML;
import scala.testing.UnitTest._ ;
import scala.xml.{Node,NodeSeq,Elem,Text};

object Test with Application {

  val e = Node.NoAttributes;

/*
  def eq( a:Seq[Node], b:Seq[Node] ):boolean = {
    (a.length == b.length) && eq(a.elements,b.elements)
  }
  def eq( ita:Iterator[Node], itb:Iterator[Node] ) = {
    var res = true;
    while( ita.hasNext && itb.hasNext && res ) {
      res = (ita.next == itb.next);
    };
    !ita.hasNext && !itb.hasNext && res
  }
  */
  val xmlFile1 = "<hello><world/></hello>";
  val isrc1 = new InputSource( new StringReader( xmlFile1 ) );
  val parsedxml1 = XML.load( isrc1 );
  val isrc11 = new InputSource( new StringReader( xmlFile1 ) );
  val parsedxml11 = XML.load( isrc11 );

  val c = new Node {
    def label = "hello";
    def namespace = "";
    def child = List(Elem("","world",e));
    def attributes = e;
  };

  assertSameElements( List( 3 ), List( 3 ));

  Console.println("equality");
  assertEquals( c, parsedxml11 );
  assertEquals( parsedxml1, parsedxml11 );
  assertSameElements( List(parsedxml1), List(parsedxml11));
  assertSameElements( Iterator.fromArray(Predef.Array(parsedxml1)).toList, List(parsedxml11));

  val x2 = "<book><author>Peter Buneman</author><author>Dan Suciu</author><title>Data on ze web</title></book>";

  val i = new InputSource( new StringReader( x2 ));
  val x2p = XML.load( i );

  assertEquals(x2p, Elem("","book",e,
                          Elem("","author",e,Text("Peter Buneman")),
                          Elem("","author",e,Text("Dan Suciu")),
                          Elem("","title",e,Text("Data on ze web"))));

  val xmlFile2 = "<bib><book><author>Peter Buneman</author><author>Dan Suciu</author><title>Data on ze web</title></book><book><author>John Mitchell</author><title>Foundations of Programming Languages</title></book></bib>";
  val isrc2 = new InputSource( new StringReader( xmlFile2 ) );
  val parsedxml2 = XML.load( isrc2 );

  // xmlFile2/book -> book,book
  Console.println("xpath \\");


  assertSameElements( parsedxml1 \ "_" ,    List( Elem("","world",e) ) );

  assertSameElements( parsedxml1 \ "world", List( Elem("","world",e) ) );

/*
  Console.println( parsedxml2 \ "_" );
  Console.println( (parsedxml2 \ "_" ).elements);
  for( val i <- (parsedxml2 \ "_" ).elements) {
    Console.println( i );
    };
  */
  assertSameElements(
      parsedxml2 \ "_" ,

      List(
        Elem("","book", e,
             Elem("","author",e,Text("Peter Buneman")),
             Elem("","author",e,Text("Dan Suciu")),
             Elem("","title",e,Text("Data on ze web"))),
        Elem("","book",e,
             Elem("","author",e,Text("John Mitchell")),
             Elem("","title",e,Text("Foundations of Programming Languages"))))
  );
  assertEquals( (parsedxml2 \ "author").length, 0 );

  assertSameElements(
      parsedxml2 \ "book",

      List(
        Elem("","book",e,
             Elem("","author",e,Text("Peter Buneman")),
             Elem("","author",e,Text("Dan Suciu")),
             Elem("","title",e,Text("Data on ze web"))),
        Elem("","book",e,
             Elem("","author",e,Text("John Mitchell")),
             Elem("","title",e,Text("Foundations of Programming Languages")))
      )
  );

  assertSameElements(

      parsedxml2 \ "_" \ "_",

    List(
      Elem("","author",e,Text("Peter Buneman")),
      Elem("","author",e,Text("Dan Suciu")),
      Elem("","title",e,Text("Data on ze web")),
      Elem("","author",e,Text("John Mitchell")),
      Elem("","title",e,Text("Foundations of Programming Languages"))
    )
  );

  assertSameElements(

      parsedxml2 \ "_" \ "author",

      List(
        Elem("","author",e,Text("Peter Buneman")),
        Elem("","author",e,Text("Dan Suciu")),
        Elem("","author",e,Text("John Mitchell"))
      )

  );

  assertSameElements( (parsedxml2 \ "_" \ "_" \ "author"), List() );

  Console.println("xpath \\\\ DESCENDANTS");

  assertSameElements(

      parsedxml2 \\ "author",

      List(
        Elem("","author",e,Text("Peter Buneman")),
        Elem("","author",e,Text("Dan Suciu")),
        Elem("","author",e,Text("John Mitchell"))
      )

 );

  assertEquals(

      new NodeSeq(List( parsedxml2 )) \\ "_",

      List(
        Elem("","bib",e,
             Elem("","book",e,
                  Elem("","author",e,Text("Peter Buneman")),
                  Elem("","author",e,Text("Dan Suciu")),
                  Elem("","title",e,Text("Data on ze web"))),
             Elem("","book",e,
                  Elem("","author",e,Text("John Mitchell")),
                  Elem("","title",e,Text("Foundations of Programming Languages")))),
        Elem("","book",e,
             Elem("","author",e,Text("Peter Buneman")),
             Elem("","author",e,Text("Dan Suciu")),
             Elem("","title",e,Text("Data on ze web"))),
        Elem("","author",e,Text("Peter Buneman")),
        Text("Peter Buneman"),
        Elem("","author",e,Text("Dan Suciu")),
        Text("Dan Suciu"),
        Elem("","title",e,Text("Data on ze web")),
        Text("Data on ze web"),
        Elem("","book",e,
             Elem("","author",e,Text("John Mitchell")),
             Elem("","title",e,Text("Foundations of Programming Languages"))),
        Elem("","author",e,Text("John Mitchell")),
        Text("John Mitchell"),
        Elem("","title",e,Text("Foundations of Programming Languages")),
        Text("Foundations of Programming Languages")
      )
  );


  assertSameElements(

      parsedxml2 \\ "title",

      List(
        Elem("","title",e,Text("Data on ze web")),
        Elem("","title",e,Text("Foundations of Programming Languages")))
  );


  Console.println("NodeSeq");
  import scala.xml.Utility.view ;

  val p = <foo><bar value="3"/><baz bazValue="8"/><bar value="5"/></foo>;

  assertSameElements(
    for( val x <- p \ "bar"; val y <- p \ "baz" ) yield {
      x.attribute("value") + y.attribute("bazValue")+ "!"
    },
    new NodeSeq(List(Text("38!"),Text("58!")))
  );

  val books =
    <bks>
      <book><title>Blabla</title></book>
      <book><title>Blubabla</title></book>
      <book><title>Baaaaaaalabla</title></book>
    </bks>;

  val reviews =
    <reviews>
      <entry><title>Blabla</title>
      <remarks>
        Hallo Welt.
      </remarks>
    </entry>
      <entry><title>Blubabla</title>
      <remarks>
        Hello Blu
      </remarks>
  </entry>
      <entry><title>Blubabla</title>
      <remarks>
        rem 2
      </remarks>
  </entry>
    </reviews>;

  Console.println( new scala.xml.PrettyPrinter(80, 5).format (
    for( val t <- books \\ "title";
         val r <- reviews \\ "entry";
         r \ "title" == t) yield
          <result>
    { t }
    { r \ "remarks" }
    </result>
  ));

  // example
  Console.println(
    for( val t @ <book><title>Blabla</title></book> <- new NodeSeq( books.child ).asList)
    yield t
  );

val phoneBook =
  <phonebook>
      <descr>
        This is the <b>phonebook</b> of the
        <a href="http://acme.org">ACME</a> corporation.
      </descr>
      <entry>
        <name>John</name>
        <phone where="work">  +41 21 693 68 67</phone>
        <phone where="mobile">+41 79 602 23 23</phone>
      </entry>
    </phonebook>;


val addrBook =
  <addrbook>
      <descr>
        This is the <b>addressbook</b> of the
        <a href="http://acme.org">ACME</a> corporation.
      </descr>
      <entry>
        <name>John</name>
        <street> Elm Street</street>
        <city>Dolphin City</city>
      </entry>
    </addrbook>;

  Console.println( new scala.xml.PrettyPrinter(80, 5).format (
    for( val t <- addrBook \\ "entry";
         val r <- phoneBook \\ "entry";
         t \ "name" == r \ "name") yield
          <result>
    { t.child }
    { r \ "phone" }
    </result>
  ));

  /* patterns */
  Console.println("patterns");
  assertEquals(<hello/> match { case <hello/> => true; case _ => false; },
               true);

  assertEquals(
     <hello foo="bar">
       <world/>
     </hello> match { case <hello>
                               <world/>
                           </hello> => true;
                      case _ => false; },
               true);

  assertEquals(
     <hello foo="bar">
       crazy text world
     </hello> match { case <hello>
                               crazy   text  world
                           </hello> => true;
                      case _ => false; },
               true);

  /* namespaces */
  Console.println("namespaces");
  val cuckoo = <cuckoo xmlns="http://cuckoo.com">
    <foo/>
    <bar/>
  </cuckoo>;
  assertEquals( cuckoo.namespace, "http://cuckoo.com");
  for( val n <- cuckoo.child ) {
    assertEquals( n.namespace, "http://cuckoo.com");
  }

  /*
  assertEquals( true, cuckoo match {
    case <cuckoo xmlns="http://cuckoo.com">
      <foo/>
      <bar/>
      </cuckoo> => true;
    case _      => false; });
*/
  assertEquals( false, cuckoo match {
    case <cuckoo>
           <foo/>
           <bar/>
         </cuckoo> => true;
    case _         => false; });

}
