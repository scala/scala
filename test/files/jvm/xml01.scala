import java.io.StringReader
import org.xml.sax.InputSource

import scala.testing.SUnit._ 
import scala.util.logging._
import scala.xml._


object Test extends App with Assert {
  val e:  scala.xml.MetaData         = Null  //Node.NoAttributes
  val sc: scala.xml.NamespaceBinding = TopScope

  val xmlFile1    = "<hello><world/></hello>";
  val isrc1       = new InputSource(new StringReader(xmlFile1))
  val parsedxml1  = XML.load(isrc1)
  val isrc11      = new InputSource(new StringReader(xmlFile1))
  val parsedxml11 = XML.load(isrc11)

  val c = new Node {
    def label = "hello"
    override def hashCode() = 
      Utility.hashCode(prefix, label, attributes.hashCode(), scope.hashCode(), child);
    def child = Elem(null, "world", e, sc);
    //def attributes = e;
    override def text = ""
  }

  println("equality")
  assertEqualsXML(c, parsedxml11)
  assertEqualsXML(parsedxml1, parsedxml11)
  assertSameElementsXML(List(parsedxml1), List(parsedxml11))
  assertSameElementsXML(Array(parsedxml1).toList, List(parsedxml11))

  val x2 = "<book><author>Peter Buneman</author><author>Dan Suciu</author><title>Data on ze web</title></book>";

  val i = new InputSource(new StringReader(x2))
  val x2p = XML.load(i)

  assertEqualsXML(x2p, Elem(null, "book"  , e, sc,
                        Elem(null, "author", e, sc,Text("Peter Buneman")), 
                        Elem(null, "author", e, sc,Text("Dan Suciu")), 
                        Elem(null, "title" , e, sc,Text("Data on ze web"))));

  val xmlFile2 = "<bib><book><author>Peter Buneman</author><author>Dan Suciu</author><title>Data on ze web</title></book><book><author>John Mitchell</author><title>Foundations of Programming Languages</title></book></bib>";
  val isrc2 = new InputSource(new StringReader(xmlFile2))
  val parsedxml2 = XML.load(isrc2)

  // xmlFile2/book -> book,book
  println("xpath \\")


  assertSameElementsXML(parsedxml1 \ "_" ,    List(Elem(null,"world", e, sc)))

  assertSameElementsXML(parsedxml1 \ "world", List(Elem(null,"world", e, sc)))

/*
  Console.println( parsedxml2 \ "_" );
  Console.println( (parsedxml2 \ "_" ).iterator);
  for( val i <- (parsedxml2 \ "_" ).iterator) {
    Console.println( i );
  };
  */

  assertSameElementsXML( 
      parsedxml2 \ "_" , 
      
      List(
        Elem(null,"book", e, sc,
             Elem(null,"author", e, sc, Text("Peter Buneman")), 
             Elem(null,"author", e, sc, Text("Dan Suciu")), 
             Elem(null,"title" , e, sc, Text("Data on ze web"))),
        Elem(null,"book",e,sc,
             Elem(null,"author",e,sc,Text("John Mitchell")),
             Elem(null,"title",e,sc,Text("Foundations of Programming Languages"))))
  );
  assertEquals( (parsedxml2 \ "author").length, 0 );

  assertSameElementsXML( 
      parsedxml2 \ "book", 
      
      List(
        Elem(null,"book",e,sc,
             Elem(null,"author", e, sc, Text("Peter Buneman")), 
             Elem(null,"author", e, sc, Text("Dan Suciu")), 
             Elem(null,"title" , e, sc, Text("Data on ze web"))),
        Elem(null,"book",e,sc,
             Elem(null,"author", e, sc, Text("John Mitchell")),
             Elem(null,"title" , e, sc, Text("Foundations of Programming Languages")))
      )
  );

  assertSameElementsXML( 

      parsedxml2 \ "_" \ "_",

    List(
      Elem(null,"author", e, sc, Text("Peter Buneman")), 
      Elem(null,"author", e, sc, Text("Dan Suciu")), 
      Elem(null,"title" , e, sc, Text("Data on ze web")),
      Elem(null,"author", e, sc, Text("John Mitchell")),
      Elem(null,"title" , e, sc, Text("Foundations of Programming Languages"))
    )
  );

  assertSameElementsXML( 

      parsedxml2 \ "_" \ "author",

      List(
        Elem(null,"author", e, sc, Text("Peter Buneman")), 
        Elem(null,"author", e, sc, Text("Dan Suciu")), 
        Elem(null,"author", e, sc, Text("John Mitchell"))
      )
    
  );

  assertSameElementsXML( (parsedxml2 \ "_" \ "_" \ "author"), List() );

  Console.println("xpath \\\\ DESCENDANTS");

  assertSameElementsXML( 

      parsedxml2 \\ "author",

      List(
        Elem(null,"author", e, sc, Text("Peter Buneman")), 
        Elem(null,"author", e, sc, Text("Dan Suciu")), 
        Elem(null,"author", e, sc, Text("John Mitchell"))
      )

 );


  assertSameElementsXML( 

      parsedxml2 \\ "title",

      List(
        Elem(null,"title", e, sc, Text("Data on ze web")),
        Elem(null,"title", e, sc, Text("Foundations of Programming Languages")))
  );


  println(
    (parsedxml2 \\ "book" ){ n:Node => (n \ "title") xml_== "Data on ze web" }
  );

  assertEqualsXML( 

      (new NodeSeq { val theSeq = List( parsedxml2 ) }) \\ "_",

      List(
        Elem(null,"bib",e,sc,
             Elem(null,"book",e,sc,
                  Elem(null, "author", e, sc, Text("Peter Buneman")), 
                  Elem(null, "author", e, sc, Text("Dan Suciu")), 
                  Elem(null, "title" , e, sc, Text("Data on ze web"))),
             Elem(null,"book",e,sc,
                  Elem(null,"author",e,sc,Text("John Mitchell")),
                  Elem(null,"title",e,sc,Text("Foundations of Programming Languages")))),
        Elem(null,"book",e,sc,
             Elem(null,"author",e,sc,Text("Peter Buneman")), 
             Elem(null,"author",e,sc,Text("Dan Suciu")), 
             Elem(null,"title",e,sc,Text("Data on ze web"))),
        Elem(null,"author",e,sc,Text("Peter Buneman")),
        //Text("Peter Buneman"),
        Elem(null,"author",e,sc,Text("Dan Suciu")),
        //Text("Dan Suciu"),
        Elem(null,"title",e,sc,Text("Data on ze web")),
        //Text("Data on ze web"),
        Elem(null,"book",e,sc,
             Elem(null,"author",e,sc,Text("John Mitchell")),
             Elem(null,"title",e,sc,Text("Foundations of Programming Languages"))),
        Elem(null,"author",e,sc,Text("John Mitchell")),
        //Text("John Mitchell"),
        Elem(null,"title",e,sc,Text("Foundations of Programming Languages"))
        //Text("Foundations of Programming Languages")
      )
  );

    // test group node
    Console println "-- group nodes"
    val zx1: Node = Group { <a/><b/><c/> }
    val zy1 = <f>{zx1}</f>
    Console println zy1.toString()

    val zx2: Node = Group { List(<a/>,zy1,zx1) }
    Console println zx2.toString()

    val zz1 = <xml:group><a/><b/><c/></xml:group>

    assertTrue(zx1 xml_== zz1)
    assertTrue(zz1.length == 3)

    // unparsed

    // val uup = <xml:unparsed>&<<>""^%@$!#</xml:unparsed>
    // assertTrue(uup == "&<<>\"\"^%@$!#")
    // test unicode escapes backslash u 

  println("attribute value normalization")
  val xmlAttrValueNorm = "<personne id='p0003' nom='&#x015e;ahingöz' />";
    {
      val isrcA       = new InputSource( new StringReader(xmlAttrValueNorm) );
      val parsedxmlA  = XML.load(isrcA);
      val c = (parsedxmlA \ "@nom").text.charAt(0);
      //Console.println("char '"+c+"' \u015e");
      assertTrue(c == '\u015e');
    }
    // buraq: if the following test fails with 'character x not allowed', it is
    //        related to the mutable variable in a closures in MarkupParser.parsecharref
    {
      val isr  = scala.io.Source.fromString(xmlAttrValueNorm);
      val pxmlB  = scala.xml.parsing.ConstructingParser.fromSource(isr,false);
      val parsedxmlB  = pxmlB.element(TopScope);
      val c = (parsedxmlB \ "@nom").text.charAt(0);
      //Console.println("char '"+c+"' \u015e");
      assertTrue(c == '\u015e');
    }

  // #60 test by round trip

  val p = scala.xml.parsing.ConstructingParser.fromSource(scala.io.Source.fromString("<foo bar:attr='&amp;'/>"),true)
  val n = p.element(new scala.xml.NamespaceBinding("bar","BAR",scala.xml.TopScope))(0)
  assertFalse( n.attributes.get("BAR", n, "attr").isEmpty)
}
