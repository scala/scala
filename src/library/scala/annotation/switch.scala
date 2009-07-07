/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */
package scala.annotation

/** <p>
 *    An annotation to be applied to a match expression.  If present,
 *    the compiler will verify that the match has been compiled to a
 *    <a href="http://java.sun.com/docs/books/jvms/second_edition/html/Instructions2.doc14.html"
 *    target="_top"><code>tableswitch</code></a> or
 *    <a href="http://java.sun.com/docs/books/jvms/second_edition/html/Instructions2.doc8.html#lookupswitch"
 *    target="_top"><code>lookupswitch</code></a>, and issue an error if it
 *    instead compiles into a series of conditional expressions.<br/>
 *    Example:
 *  </p>
 *  <pre>
 *    <b>def</b> fetchToken() {
 *      (ch: @switch) <b>match</b> {
 *        <b>case</b> ' ' | '\t' | CR | LF | FF <b>=&gt;</b>
 *          nextChar()
 *          fetchToken()
 *        <b>case</b> 'A' &#47;*..'Z'*&#47; | '$' | '_' | 'a' &#47;*..'z'*&#47; <b>=&gt;</b>
 *          putChar(ch)
 *          nextChar()
 *          getIdentRest()
 *        <b>case</b> ',' <b>=&gt;</b>
 *          nextChar(); token = COMMA
 *        // more cases
 *      }
 *    }</pre>
 */
final class switch extends StaticAnnotation
