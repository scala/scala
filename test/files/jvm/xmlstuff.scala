import java.io.StringReader;
import org.xml.sax.InputSource;
import scala.xml.nobinding.XML;
import scala.testing.UnitTest._ ;
import scala.xml.{Node,NodeSeq,Elem,Text};

object Test with Application {

  //val e:  scala.xml.MetaData         = null; //Node.NoAttributes;
  //val sc: scala.xml.NamespaceBinding = null;


  Console.println("NodeSeq");
  import scala.xml.Utility.view ;

  val p = <foo><bar value="3"/><baz bazValue="8"/><bar value="5"/></foo>;

  assertSameElements(
    for( val x <- p \ "bar"; val y <- p \ "baz" ) yield {
      x.attribute("value") + y.attribute("bazValue")+ "!"
    },
    new NodeSeq { val theSeq = List(Text("38!"),Text("58!")) }
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
}
