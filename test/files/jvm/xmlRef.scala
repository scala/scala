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

  val x1s = <foo>&#0123;</foo>.toString();

  Console.println( x1s );

  val x2 = <foo>&nbsp;&#x7b;<bar><baz/></bar><bar/></foo>.toString();

  Console.println( x2 );

}
