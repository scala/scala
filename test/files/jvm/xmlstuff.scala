import java.io.StringReader;
import org.xml.sax.InputSource;
import scala.xml.nobinding.XML;
import scala.testing.UnitTest._ ;
import scala.xml.{Node,NodeSeq};

object Test with Application {

  def eq( a:Seq[Node], b:Seq[Node] ) = {
    if( a.length == b.length ) {
      val ita = a.elements;
      val itb = b.elements;
      var res = true;
      while( ita.hasNext ) {
        res = res &&( ita.next == itb.next );
      };
      res
    } else {
      false
    }
  }
  val xmlFile1 = "<hello><world/></hello>";
  val isrc1 = new InputSource( new StringReader( xmlFile1 ) );
  val parsedxml1 = XML.load( isrc1 );

  val xmlFile2 = "<bib><book><author>Peter Buneman</author><author>Dan Suciu</author><title>Data on ze web</title></book><book><author>John Mitchell</author><title>Foundations of Programming Languages</title></book></bib>";
  val isrc2 = new InputSource( new StringReader( xmlFile2 ) );
  val parsedxml2 = XML.load( isrc2 );

  // xmlFile2/book -> book,book

  assertEquals( eq( parsedxml1/'_ ,List('world()) ), true);
  assertEquals( eq( parsedxml1/'world ,List('world()) ), true);

  assertEquals(
    eq(

      parsedxml2/'_,

      List(
        'book('author(Text("Peter Buneman")),
              'author(Text("Dan Suciu")),
              'title(Text("Data on ze web"))),
        'book('author(Text("John Mitchell")),
              'title(Text("Foundations of Programming Languages")))
      )), true );


  assertEquals( (parsedxml2/'author).length == 0, true );


  assertEquals(
    eq(

      parsedxml2/'book,

      List(
        'book('author(Text("Peter Buneman")),
              'author(Text("Dan Suciu")),
              'title(Text("Data on ze web"))),
        'book('author(Text("John Mitchell")),
              'title(Text("Foundations of Programming Languages")))
      )), true );

  assertEquals(
    eq(

      parsedxml2/'_/'_,

      List('author(Text("Peter Buneman")),
           'author(Text("Dan Suciu")),
           'title(Text("Data on ze web")),
           'author(Text("John Mitchell")),
           'title(Text("Foundations of Programming Languages")))

    ), true);

  assertEquals(
    eq(

      parsedxml2/'_/'author,

      List('author(Text("Peter Buneman")),
           'author(Text("Dan Suciu")),
           'author(Text("John Mitchell")))

    ), true);

  assertEquals( (parsedxml2/'_/'_/'author).length == 0, true );

  assertEquals(
    eq(

      parsedxml2/#'author,

      List('author(Text("Peter Buneman")),
           'author(Text("Dan Suciu")),
           'author(Text("John Mitchell")))

    ), true );

  assertEquals(
    eq(

      parsedxml2/#'_,

      List(
        'book('author(Text("Peter Buneman")),
              'author(Text("Dan Suciu")),
              'title(Text("Data on ze web"))),
        'author(Text("Peter Buneman")),
        Text("Peter Buneman"),
        'author(Text("Dan Suciu")),
        Text("Dan Suciu"),
        'title(Text("Data on ze web")),
        Text("Data on ze web"),
        'book('author(Text("John Mitchell")),
              'title(Text("Foundations of Programming Languages"))),
        'author(Text("John Mitchell")),
        Text("John Mitchell"),
        'title(Text("Foundations of Programming Languages")),
        Text("Foundations of Programming Languages"))
    ) , true);


  assertEquals(
    eq(

      parsedxml2/#'title,

      List(
        'title(Text("Data on ze web")),
        'title(Text("Foundations of Programming Languages")))
    ) , true);

}
