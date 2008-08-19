/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2007, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.reflect

/** <p>
 *    This attribute adds a setter and a getter method, following the
 *    Java Bean convention (first letter of the property is capitalized)
 *    used by popular Java web frameworks. For example:
 *  </p><pre>
 *    @BeanProperty
 *    <b>var</b> status = ""</pre>
 *  <p>
 *    adds the following methods to the <b>generated</b> code
 *  </p><pre>
 *    <b>def</b> setStatus(s: String) { <b>this</b>.status = s }
 *    <b>def</b> getStatus: String = <b>this</b>.status
 *  </pre>
 *  <p>
 *    However, you cannot call <code>setStatus</code> from
 *    <a href="http://scala-lang.org/" target="_top">Scala</a>,
 *    you should use the normal Scala access and assignment.
 *  </p>
 */
class BeanProperty extends StaticAnnotation
