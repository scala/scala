//############################################################################
// XML Parsing
//############################################################################
// $Id$

import scala.testing.UnitTest._ ;

import scala.xml._ ;

object Test with Application {

  // error: no whitespace between attributes
  val y = <hello foo="bar"baz="baz"></hello>.toString();

}
