/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.testing;

/** some simple methods to support unit testing with assertions
**  to contain more JUnit style assertions which use Scala's features.
*/
object UnitTest {

  import java.lang.System;

  def message_passedOK:Unit = {
    System.out.println("passed ok")
  }

  def message_failed( actual:String, expected:String ):Unit = {
    System.out.print("failed! we got");
    System.out.print( "\""+ actual +"\"" );
    System.out.println(" but expected " + expected)
  }

  def assertEquals[a]( actual: a, expected: a ):Unit =
    if( actual == expected )
        message_passedOK
    else
	message_failed( actual.toString(), expected.toString() );

  def assertNotEquals[a]( actual: a, expected: a ):Unit =
    if( actual != expected )
        message_passedOK
    else
	message_failed( actual.toString(), "something != "+expected.toString() );

  def test[a]( def doit: a, expected: a ):Unit = assertEquals( doit, expected );

} // unitTest
