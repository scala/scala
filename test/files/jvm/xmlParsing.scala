//############################################################################
// XML Parsing
//############################################################################
// $Id$

import scala.testing.UnitTest._ ;

import scala.xml._ ;

/** this file test just succesful run of the parser. There are no checks whether
 *  the encoding to Scala expressions is done correctly
 */
object Test with Application {

  val x0  = <hello/>;
  val x1s = <foo></foo>.toString();
  val x2 = <foo><bar><baz/></bar><bar/></foo>.toString();

  /* whitespace (ws) handling */

  val x3 = <hello  >  </hello>.toString();       /* ws in tags allowed */

  val x4 = <hello>
             <world></world>
             <test/>
             <mars></mars></hello>.toString();   /* ws in element content */

  /* attributes */

  val z = <html>
            <body background="#FFFFFF">
              <h1>Hello World</h1>
              <p>Check the <a href="scala.epfl.ch">scala</a> page!</p>
            </body>
          </html>.toString();

  val ent = <foo>
                 hello   &nbsp; character entities!
                 welcome &#0160; unicode characters!
            </foo>;


  val foo = <a/><b/><c/>;


  val foo2 = foo match {
    case <a/><b/> => 1
    case <a></a><b></b><c></c> => 2
    case Seq(Elem("a",_),Elem("b",_),Elem("c",_)) => 3
  };

}
