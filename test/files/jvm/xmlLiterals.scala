//############################################################################
// XML Literals
//############################################################################
// $Id$

import scala.testing.UnitTest._ ;

import scala.xml._ ;
import scala.collection.immutable ;


object Test  {

    val e = immutable.TreeMap.Empty[String,String];

    /* a helper function to compare up to whitespace */

    def noWS(x:String):String = {
      val res = new StringBuffer();
      var i = 0; while( i < x.length() ) {
        val c = x.charAt( i );
        c match {
          case ' ' | '\n' | '\t' =>
            case _ => res.append( c )
        }
        i = i + 1;
      }
      res.toString();
    }


  object Test01Literals {



    /*                                             === tags, elements === */

    assertEquals( <hello></hello>.toString(),
                  <hello  ></hello>.toString());           /* ws in tags */

    assertEquals( <hello></hello>.toString(),
                  noWS(<hello  >   </hello>.toString()));

    val x3 = <hello>
    <world></world>
    <test/>
    <mars></mars></hello>.toString();          /* ws in element content */


    assertEquals( noWS( x3 ),
                  Elem("hello",e,
                       Elem("world",e),
                       Elem("test",e),
                       Elem("mars",e)).toString() );


  /*                                                === attributes === */

    val z = <html>
            <body background="#FFFFFF">
              <h1>Hello World</h1>
              <p>Check the <a href="scala.epfl.ch">scala</a> page!</p>
            </body>
          </html>.toString();

    assertEquals( noWS( z ), noWS(
      Elem("html",e,
           Elem("body",e,
                Elem("h1",e,Text("Hello World")),
                Elem("p",e,Text("Check the "),
                     Elem("a", e,Text("scala"))
                     % Pair("href","scala.epfl.ch"),
                     Text("page!"))
              ) % Pair("background","#FFFFFF")
         ).toString()
    ));

    /*                                  === attributes are Scala expr === */

    val testValue = "This is a test.";
    val testValue2 = 42;
    val page = <testTag value={ testValue } ab="bkla" />;
    val page2 = <testTag value={testValue2.toString()} bla="foo"></testTag>;

    Console.println( page.toString() );
    Console.println( page2.toString() );

  }

  object Test02Embed {

    /*                                    === embedded Scala blocks  === */

    def computeDate() = {
      Elem("date", e, Text("now!"));
    }
    /* embedding Scala strings as text and elements  */
    val sc = <hello>{ "World" }{ Text("42") }{ computeDate() }</hello>;

    assertEquals( sc.child.elements.toList,
                 List( Text("World"), Text("42"), Elem( "date", e,Text("now!") ) ) );

    assertEquals( sc.toString(),
                 Elem("hello",e,Text("World42"),Elem("date",e,Text("now!"))).toString() );

    def foo( m:Node ):String = m match {
      case <hello/> => "hello node"
      case <hallo ></hallo > => "hallo node"
      case <test>{ z }</test> => "test node:"+z
      case <list>{ e1:Node }{ e2:Node }{ _* }</list> => e1.toString() + e2.toString();
    }

    assertEquals( foo(<hello/>), "hello node" );
    assertEquals( foo(<hallo/>), "hallo node" );
    assertEquals( foo(<test>42</test>), "test node:42" );
    assertEquals( foo(<list><a/><b><c/></b><d/><d/></list>),
     	         <a/>.toString() + <b><c/></b>.toString() );

    val rows = <tr>
		<td>1.1</td><td>1.2</td>
             </tr>
             <tr>
		<td>2.1</td><td>2.2</td>
             </tr>;

    assertEquals( noWS( rows.toList.toString() ),
                 noWS( List(Elem("tr",e,
                                 Elem("td",e,Text("1.1")),Elem("td",e,Text("1.2"))
                               ),
		            Elem("tr",e,
                                 Elem("td",e,Text("2.1")),Elem("td",e,Text("2.2"))
                               )
                          ).toString() )
               );
    val rows2 = <tr><!-- an XML comment --><?pinotext?><?pi text?></tr>;
    val rows3 = <tr> a <!-- an XML comment --> b <?pinotext?> c <?pi text?> d </tr>;

    // these are not equal as comments are valid XML Info items.
    assertEquals( rows2, Elem("tr",e,Comment(" an XML comment "),ProcInstr("pinotext",None),ProcInstr("pi",Some("text"))));

    assertEquals( rows3, Elem("tr",e,Text(" a "),Comment(" an XML comment "),Text(" b "),ProcInstr("pinotext",None),Text(" c "),ProcInstr("pi",Some("text")),Text(" d ")));

  }

object Test03Servlet {

  val headerMsg = Text("What follows is an example of modular formatting.");
  val footerMsg = Text("Complicated layout tasks can be encapsulated and outsourced.");

  /** helper function for the main page, provides header and a footer
   */
  def page( ns:Seq[Node] ) = {
    <html>
      <head>
        <title>ModularFormatting</title>
      </head>
      <body>
        <h2>Welcome</h2>
        <p>
          { headerMsg }
        </p>
        <p>
          { ns:_* }
        </p>
        <hr/>
        <p>
          { footerMsg }
        </p>
        <h2>Bye!</h2>
      </body>
    </html>
  }

  /** applies beautify to every element in a sequence
   */
  def beautify( xs:Seq[Node] ):Seq[Node] = xs.toList.map { beautify }

  /** this is a recursive procedure that adds some attributes to the tree
   */
  def beautify( n:Node ):Node = n match {
    case <td>{ xs @ _* }</td> =>
          <td bgcolor="#AAAAFF" color="#222255">{ xs:_* }</td>

    case <table>{ xs @ _* }</table> =>
          <table align="center">{ beautify( xs ):_* }</table>

    case Elem( label, _, xs @ _* ) =>
          new Elem( label, beautify( xs ):_*)

    case _ => n
  }

  /** this function will take a node and put it in a table
   */
  def format( msg:Node ):Node = {
    <table>
      <tr>
        <td>{ msg }</td>
      </tr>
    </table>
  }

  /** returns a highlighted text node with the string given as arguemnt. if it
   *  is null, supplies a default string.
   */
  def getMessage( x:String ) = {
    if( x == null )
      <h1> This could be YOUR message ! </h1>
    else
      <h1> { Text( x ) } </h1>
  }

  /** the entry method
   */
  def doGetXML() = {
    beautify( page( List( format( getMessage( "message" ) )) ));
    /* page( List( format( theMessage ))); */

  }

  def main( args:Array[String] ) = {
    val x = doGetXML();
    Console.println( x );
    Console.println( new PrettyPrinter( 80, 2 ).toPrettyXML( x ));
  }

}


  object Test04 {
    val sequence = <foo/>
                 <bar>Text</bar>
                 <foo/>;

    Console.println( sequence );

    val onlyOne = <foo/>;

    Console.println( onlyOne );

    val tryBrace = <try>Now escaped {{ braces } </try>;
    assertEquals( tryBrace, Elem("try",e,Text("Now escaped { braces }")));

    val tryBrace2 = <try myAttrib={ (3+4).toString() }> cool ?</try>;
    assertEquals( tryBrace2.attribute("myAttrib"), "7" );

    /* Scala comments are not allowed in XML literals. see neg(2) */
    val zzz = <hello>/* no comment */</hello>;
    assertEquals( zzz, Elem("hello", e, Text("/* no comment */")));

  }


  def test05main = {
    val x1s = <foo>&#0123;</foo>.toString();
    Console.println( x1s );
    val x2 = <foo>&nbsp;&#x7b;<bar><baz/></bar><bar/></foo>.toString();
    Console.println( x2 );
  }

  def main( args:Array[String] ):Unit = {
    Console.println("Test01Literals");
    Test01Literals;
    Console.println("Test02Embed");
    Test02Embed;
    Console.println("Test03Servlet");
    Test03Servlet.main( args );
    Console.println("Test04");
    Test04;
    Console.println("Test05Ref");
    test05main;{
    }
  }


}

