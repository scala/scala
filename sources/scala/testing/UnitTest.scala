// $Id$

package scala.testing;

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

  def testEquals[a]( actual: a, expected: a ):Unit = {

    if( actual == expected )
      {
        message_passedOK
      }
    else
      {
	message_failed( actual.toString(), expected.toString() )
      }

  } // testEquals

  def testNotEquals[a]( actual: a, expected: a ):Unit = {

    if( actual != expected )
      {
        message_passedOK
      }
    else
      {
	message_failed( actual.toString(), "something != "+expected.toString() )
      }

  } // testNotEquals

  def test[b,a]( doit:b => a, input: b, expected:a ):Unit = {
    val actual = doit( input );
    if( actual == expected )
      {
        message_passedOK
      }
    else
      {
	message_failed( actual.toString(), expected.toString() )
      }
  } // test

  def test2[c,b,a]( doit:(c,b) => a, in1: c, in2: b, expected:a ): Unit = {
    val actual = doit( in1, in2 );
    if( actual == expected )
      {
        message_passedOK;
      }
    else
      {
	message_failed( actual.toString(), expected.toString() )
      }

  } // test2

} // unitTest
