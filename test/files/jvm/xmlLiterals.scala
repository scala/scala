import scala.testing.UnitTest._ ;

object Test with Application {

  /*                                                                    */
  /*                                             === tags, elements === */
  /*                                                                    */

  val x = <hello></hello>.toXML; /* see neg(1) */

  /* whitespace (ws) handling */

  val x2 = <hello  >  </hello>.toXML;       /* ws in tags allowed */

  assertEquals( x, x2 );

  val x3 = <hello>
             <world> </world>
             <test/>
             <mars></mars></hello>.toXML;   /* ws in element content */

  /* Scala comments are not allowed in XML literals. see neg(2) */

  assertEquals( x3, 'hello('world,'test,'mars).toXML );

  /*                                                                   */
  /*                                                === attributes === */
  /*                                                                   */

  val z = <html>
            <body background="#FFFFFF">
              <h1>Hello World</h1>
              <p>Check the <a href="scala.epfl.ch">scala</a> page!</p>
            </body>
          </html>.toXML;

  assertEquals( z, 'html(
                      'body(
                          'h1("Hello World"),
                          'p("Check the ",
                             'a("scala") % ('href <= "scala.epfl.ch" ),
                             "page!")) %('background <= "#FFFFFF")).toXML);

  /*  todo: better way to deal with whitespace in content              */
  /*      (Canonical XML or even more aggressive normlization)         */

  /*                                                                   */
  /*                                    === embedded Scala blocks  === */
  /*                                                                   */

  def computeDate() = {
    'date("now!")
  }


  /* embedding Scala strings as text and elements  */
  val sc = <hello> { "World" }{ 7*6 }{ computeDate() }</hello>;

  assertEquals( sc.children.toList, List(Text("World"),Text("42"), 'date("now!")) );
  assertEquals( sc.toXML, 'hello("World42",'date("now!")).toXML );

  /* examples that MUST fail

  neg(1)
  val y = <hello></hallo>.toXML; // error: closing tag of hello

  neg(2)
  val z = <hello> // my hello comment      <--- this will be parsed as text !!
          </hello>

  */


}

