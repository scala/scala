import java.io.StringReader;
import org.xml.sax.InputSource;
import scala.xml.nobinding.XML;
import scala.testing.UnitTest.assertEquals ;
import scala.xml.NodeList;

object Test with Application {
  val xmlFile1 = "<hello><world/></hello>";
  val isrc1 = new InputSource( new StringReader( xmlFile1 ) );
  val parsedxml1 = XML.load( isrc1 );

  val xmlFile2 = "<bib><book><author>Peter Buneman</author><author>Dan Suciu</author><title>Data on ze web</title></book><book><author>John Mitchell</author><title>Foundations of Programming Languages</title></book></bib>";
  val isrc2 = new InputSource( new StringReader( xmlFile2 ) );
  val parsedxml2 = XML.load( isrc2 );

  // xmlFile2/book -> book,book

  Console.println( parsedxml1/'world ); /* List('world()) */
  Console.println( parsedxml1/'_ );     /* List('world()) */

  Console.println( "\nparsedxml2/'_"); /* List(book,book) */
  Console.println( parsedxml2/'_); /* List(book,book) */

  Console.println( "\nparsedxml2/'author"); /* List() */
  Console.println( parsedxml2/'author); /* List() */

  Console.println( "\nparsedxml2/'book:");
  Console.println( parsedxml2/'book); /* List(book,book) */

  Console.println( "\nparsedxml2/'_/'_");
  Console.println( parsedxml2/'_/'_ );
  Console.println( "\nparsedxml2/'_/'author");
  Console.println( parsedxml2/'_/'author );
  Console.println( "\nparsedxml2/'_/'_/'author");
  Console.println( parsedxml2/'_/'_/'author );
               /* List('author(Text("Peter Buneman")),
                    'author(Text("Dan Suciu")),
                    'author(Text("John Mitchell")))); */

  Console.println( "\nparsedxml2/#'author");
  Console.println( parsedxml2/#'author );

  Console.println( "\nnew NodeList(List(parsedxml2))/#'_");
  Console.println( new NodeList( List( parsedxml2 ))/#'_ );

  Console.println( "\nnew NodeList(List(parsedxml2))/#'title");
  Console.println( new NodeList( List( parsedxml2 ))/#'title );


}
