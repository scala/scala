//############################################################################
// XML Parsing
//############################################################################
// $Id$

import scala.testing.UnitTest._ ;

import scala.xml._ ;

object Test with Application {

  // error: closing tag of hello not found
  val y = <hello></hallo>.toString();

}
