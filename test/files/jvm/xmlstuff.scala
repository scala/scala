import java.io.StringReader;
import org.xml.sax.InputSource;
import scala.xml.nobinding.XML;
import scala.testing.UnitTest._ ;
import scala.xml.{Node,NodeSeq,Elem,Text};

object Test with Application {

  val e = scala.collection.immutable.TreeMap.Empty[String,String];

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
    val namespaceCode = 0;
    def child = List(Elem(0,"world",e));
    def attribute = e;
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

  assertEquals(x2p, Elem(0,"book",e,
                          Elem(0,"author",e,Text("Peter Buneman")),
                          Elem(0,"author",e,Text("Dan Suciu")),
                          Elem(0,"title",e,Text("Data on ze web"))));

  val xmlFile2 = "<bib><book><author>Peter Buneman</author><author>Dan Suciu</author><title>Data on ze web</title></book><book><author>John Mitchell</author><title>Foundations of Programming Languages</title></book></bib>";
  val isrc2 = new InputSource( new StringReader( xmlFile2 ) );
  val parsedxml2 = XML.load( isrc2 );

  // xmlFile2/book -> book,book
  Console.println("xpath \\");


  assertSameElements( parsedxml1 \ "_" ,    List( Elem(0,"world",e) ) );

  assertSameElements( parsedxml1 \ "world", List( Elem(0,"world",e) ) );

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
        Elem(0,"book", e,
             Elem(0,"author",e,Text("Peter Buneman")),
             Elem(0,"author",e,Text("Dan Suciu")),
             Elem(0,"title",e,Text("Data on ze web"))),
        Elem(0,"book",e,
             Elem(0,"author",e,Text("John Mitchell")),
             Elem(0,"title",e,Text("Foundations of Programming Languages"))))
  );
  assertEquals( (parsedxml2 \ "author").length, 0 );

  assertSameElements(
      parsedxml2 \ "book",

      List(
        Elem(0,"book",e,
             Elem(0,"author",e,Text("Peter Buneman")),
             Elem(0,"author",e,Text("Dan Suciu")),
             Elem(0,"title",e,Text("Data on ze web"))),
        Elem(0,"book",e,
             Elem(0,"author",e,Text("John Mitchell")),
             Elem(0,"title",e,Text("Foundations of Programming Languages")))
      )
  );

  assertSameElements(

      parsedxml2 \ "_" \ "_",

    List(
      Elem(0,"author",e,Text("Peter Buneman")),
      Elem(0,"author",e,Text("Dan Suciu")),
      Elem(0,"title",e,Text("Data on ze web")),
      Elem(0,"author",e,Text("John Mitchell")),
      Elem(0,"title",e,Text("Foundations of Programming Languages"))
    )
  );

  assertSameElements(

      parsedxml2 \ "_" \ "author",

      List(
        Elem(0,"author",e,Text("Peter Buneman")),
        Elem(0,"author",e,Text("Dan Suciu")),
        Elem(0,"author",e,Text("John Mitchell"))
      )

  );

  assertSameElements( (parsedxml2 \ "_" \ "_" \ "author"), List() );

  Console.println("xpath \\\\ DESCENDANTS");

  assertSameElements(

      parsedxml2 \\ "author",

      List(
        Elem(0,"author",e,Text("Peter Buneman")),
        Elem(0,"author",e,Text("Dan Suciu")),
        Elem(0,"author",e,Text("John Mitchell"))
      )

 );

  assertEquals(

      new NodeSeq(List( parsedxml2 )) \\ "_",

      List(
        Elem(0,"bib",e,
             Elem(0,"book",e,
                  Elem(0,"author",e,Text("Peter Buneman")),
                  Elem(0,"author",e,Text("Dan Suciu")),
                  Elem(0,"title",e,Text("Data on ze web"))),
             Elem(0,"book",e,
                  Elem(0,"author",e,Text("John Mitchell")),
                  Elem(0,"title",e,Text("Foundations of Programming Languages")))),
        Elem(0,"book",e,
             Elem(0,"author",e,Text("Peter Buneman")),
             Elem(0,"author",e,Text("Dan Suciu")),
             Elem(0,"title",e,Text("Data on ze web"))),
        Elem(0,"author",e,Text("Peter Buneman")),
        Text("Peter Buneman"),
        Elem(0,"author",e,Text("Dan Suciu")),
        Text("Dan Suciu"),
        Elem(0,"title",e,Text("Data on ze web")),
        Text("Data on ze web"),
        Elem(0,"book",e,
             Elem(0,"author",e,Text("John Mitchell")),
             Elem(0,"title",e,Text("Foundations of Programming Languages"))),
        Elem(0,"author",e,Text("John Mitchell")),
        Text("John Mitchell"),
        Elem(0,"title",e,Text("Foundations of Programming Languages")),
        Text("Foundations of Programming Languages")
      )
  );


  assertSameElements(

      parsedxml2 \\ "title",

      List(
        Elem(0,"title",e,Text("Data on ze web")),
        Elem(0,"title",e,Text("Foundations of Programming Languages")))
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

}
