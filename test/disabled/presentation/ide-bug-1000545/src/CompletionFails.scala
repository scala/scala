/**
 * Presentation compiler returns the wrong answer for this test.
 * 
 * Below is the current result of running this test:
 * 
 * Mircos-iMac:test mirco$  ./partest files/presentation/ticket-1000545 --show-log | sed 's/< //'
 * Testing individual files
 * testing: [...]/files/presentation/ticket-1000545                      [FAILED]
 * 1,8d0
 * reload: CompletionFails.scala
 * 
 * askTypeCompletion at CompletionFails.scala(2,19)
 * ================================================================================
 * [response] aksTypeCompletion at (2,19)
 * retrieved 1 members
 * TypeMember(method <clinit>,()Unit,false,false,<none>)
 * ================================================================================
 * 
 * 1 of 1 tests failed (elapsed time: 00:00:05)
 * 
 * @note The expected result was the list of static methods for class @see java.io.Console
 */
object CompletionFails {
  java.io.Console. /*!*/
}