import java.io.StringReader;
import org.xml.sax.InputSource;
import scala.testing.UnitTest._ ;
import scala.xml.{Node, NodeSeq, Elem, Text, XML};

object Test {

  /** returns true if exception was thrown */
  def catcher(att:Function1[Unit,scala.xml.MetaData]): Boolean = {
    var ex = false;
      try {
        val x = att.apply({});
      } catch {
        case scala.xml.MalformedAttributeException(msg) =>
          Console.println(msg);
          ex = true;
      }
    return ex;
  }

  def main(args:Array[String]) = {

  //val e:  scala.xml.MetaData         = null; //Node.NoAttributes;
  //val sc: scala.xml.NamespaceBinding = null;

    // ------------------------------------------ tests for class NodeSeq

    Console.println("checking wellformed attributes");
    {
    import scala.xml.{ UnprefixedAttribute, Null }
    assertTrue(catcher {x:Unit => new UnprefixedAttribute("key", "<", Null)});   // < illegal
    assertTrue(catcher(x:Unit => new UnprefixedAttribute("key", "&", Null)));   // & illegal
    assertTrue(catcher(x:Unit => new UnprefixedAttribute("key", "a&a", Null))); // & illegal
    assertTrue(catcher(x:Unit => new UnprefixedAttribute("key", "a&a;&", Null))); // 2nd &

    assertFalse(catcher(x:Unit => new UnprefixedAttribute("key", "a&a; &lt;&lt;", Null)));
    }


  Console.println("NodeSeq");
  import scala.xml.Utility.view ;

    val p = <foo>
              <bar gt='ga' value="3"/>
              <baz bazValue="8"/>
              <bar value="5" gi='go'/>
            </foo>;

    val pelems_1 = for( val x <- p \ "bar"; val y <- p \ "baz" ) yield {
      Text(x.attribute("value") + y.attribute("bazValue")+ "!")
    };
    val pelems_2 = new NodeSeq { val theSeq = List(Text("38!"),Text("58!")) };
    assertSameElements(pelems_1, pelems_2);
    assertSameElements(
      p \\ "@value", new NodeSeq { val theSeq = List(Text("3"), Text("5")) }
    );


    /* // more test cases !!!
    val test = <a name="bar"/>;

    Console.println(test \ "@name");

    val x = test.attributes.nodes;
    Console.println("trying to print");
    val it = x.elements;
    while(it.hasNext) {
      val c = it.next;
      Console.println(c);
      Console.println("c.label == @name? "+(c.label == "@name"));
    }
    */

    /*
    for(val c <- x) {
      Console.println(x);
    }
    */

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

  Console.println( new scala.xml.PrettyPrinter(80, 5).formatNodes (
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
    for( val t @ <book><title>Blabla</title></book> <- new NodeSeq { val theSeq = books.child }.asList)
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

  Console.println( new scala.xml.PrettyPrinter(80, 5).formatNodes (
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
     </hello> match { case x @ <hello>
                               <world/>
                           </hello> if x.attribute("foo") == "bar" => true;
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
   // begin tmp
  Console.println("namespaces");
  val cuckoo = <cuckoo xmlns="http://cuckoo.com">
    <foo/>
    <bar/>
  </cuckoo>;
  assertEquals( cuckoo.namespace, "http://cuckoo.com");
  for( val n <- cuckoo.child ) {
    assertEquals( n.namespace, "http://cuckoo.com");
  }
  // end tmp
  /*

DEPRECATED, don't support namespaces in pattern match anymore
*/
  /*
  assertEquals( true, cuckoo match {
    case <cuckoo xmlns="http://cuckoo.com">
      <foo/>
      <bar/>
      </cuckoo> => true;
    case _      => false; });
*/
  /*
   // begin tmp
  assertEquals( false, cuckoo match {
    case <cuckoo>
           <foo/>
           <bar/>
         </cuckoo> => true;
    case _         => false; });
    // end tmp
    */
  Console.println("validation - elements");
  val vtor = new scala.xml.dtd.ElementValidator();
  {
    import scala.xml.dtd.ELEMENTS;
    import scala.xml.dtd.ContentModel._;
    vtor.setContentModel(
	  ELEMENTS(
	    Sequ(
		  Letter(ElemName("bar")),
		  Star(Letter(ElemName("baz"))) )));

  }
  assertEquals( vtor( <foo><bar/><baz/><baz/></foo> ), true );
  {
    import scala.xml.dtd.MIXED;
    import scala.xml.dtd.ContentModel._;

    vtor.setContentModel(
      MIXED(
        Alt(Letter(ElemName("bar")),
            Letter(ElemName("baz")),
            Letter(ElemName("bal")))));
  }

  assertEquals( vtor( <foo><bar/><baz/><baz/></foo> ), true );
  assertEquals( vtor( <foo>ab<bar/>cd<baz/>ed<baz/>gh</foo> ), true );
  assertEquals( vtor( <foo> <ugha/> <bugha/> </foo> ), false );

  Console.println("validation - attributes");
  vtor.setContentModel(null);
  vtor.setMetaData(List());
  assertEquals( vtor( <foo bar="hello"/> ), false );

  {
    import scala.xml.dtd._ ;
    vtor.setMetaData(List(AttrDecl("bar","CDATA",IMPLIED)));
  }
  assertEquals( vtor( <foo href="http://foo.com" bar="hello"/> ), false );
  assertEquals( vtor( <foo bar="hello"/> ), true );

  {
    import scala.xml.dtd._ ;
    vtor.setMetaData(List(AttrDecl("bar","CDATA",REQUIRED)));
  }
  assertEquals( vtor( <foo href="http://foo.com" /> ), false );
  assertEquals( vtor( <foo bar="http://foo.com" /> ), true );

  }
}
