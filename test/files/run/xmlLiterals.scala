//############################################################################
// XML Literals
//############################################################################
// $Id$

import scala.testing.UnitTest._ ;

import scala.xml._ ;

object Test with Application {

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



  /*                                                                    */
  /*                                             === tags, elements === */
  /*                                                                    */

  val x = <hello></hello>.toString(); /* see neg(1) */

  /* whitespace (ws) handling */

  val x2 = <hello  >  </hello>.toString();       /* ws in tags allowed */

  assertEquals( x, noWS( x2 ) );

  val x3 = <hello>
             <world></world>
             <test/>
             <mars></mars></hello>.toString();   /* ws in element content */


  assertEquals( noWS( x3 ),
               Elem("hello",
                    Elem("world"),
                    Elem("test"),
                    Elem("mars")).toString() );


  /*                                                                   */
  /*                                                === attributes === */
  /*                                                                   */

  val z = <html>
            <body background="#FFFFFF">
              <h1>Hello World</h1>
              <p>Check the <a href="scala.epfl.ch">scala</a> page!</p>
            </body>
          </html>.toString();

  assertEquals( noWS( z ), noWS(
    Elem("html",
         Elem("body",
              Elem("h1",Text("Hello World")),
              Elem("p",Text("Check the "),
                   Elem("a", Text("scala"))
                     % Pair("href","scala.epfl.ch"),
                   Text("page!"))
            ) % Pair("background","#FFFFFF")
           ).toString()
  ));

  /*  todo: better way to deal with whitespace in content              */
  /*      (Canonical XML or even more aggressive normlization)         */

  /*                                                                   */
  /*                                    === embedded Scala blocks  === */
  /*                                                                   */

  def computeDate() = {
    Elem("date", Text("now!"));
  }


  /* embedding Scala strings as text and elements  */
  val sc = <hello>{ "World" }{ Text("42") }{ computeDate() }</hello>;

  assertEquals( sc.child.elements.toList,
               List( Text("World"), Text("42"), Elem( "date", Text("now!") ) ) );

  assertEquals( sc.toString(),
                Elem("hello",Text("World42"),Elem("date",Text("now!"))).toString() );

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
               noWS( List(Elem("tr",
                         Elem("td",Text("1.1")),Elem("td",Text("1.2"))
                       ),
		    Elem("tr",
                         Elem("td",Text("2.1")),Elem("td",Text("2.2"))
                       )
                  ).toString() )
               );
  val rows2 = <tr><!-- an XML comment --><td>1.1</td><td>1.2</td>
             </tr>
             <tr>
		<td>2.1</td><td>2.2</td>
             </tr>;

  assertEquals( noWS( rows.toList.toString() ), noWS( rows2.toList.toString() ) );

  val sequence = <foo/>
                 <bar>Text</bar>
                 <foo/>;

  Console.println( sequence );

  val onlyOne = <foo/>;

  Console.println( onlyOne );

  val tryBrace = <try>Now we try escaped {{ braces } </try>;

  assertEquals( tryBrace, Elem("try",Text("Now we try escaped { braces }")));

  val tryBrace2 = <try myAttrib={ (3+4).toString() }> cool ?</try>;

  assertEquals( tryBrace2("myAttrib").get, "7" );

  /* Scala comments are not allowed in XML literals. see neg(2) */
  val zzz = <hello>/* no comment */</hello>;
  assertEquals( zzz, Elem("hello", Text("/* no comment */")));

}

