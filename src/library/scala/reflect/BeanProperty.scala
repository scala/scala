/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.reflect

/** <p>
 *    When attached to a field, this annotation adds a setter and a getter
 *    method following the  Java Bean convention. For example:
 *  </p><pre>
 *    @BeanProperty
 *    <b>var</b> status = ""</pre>
 *  <p>
 *    adds the following methods to the class:
 *  </p><pre>
 *    <b>def</b> setStatus(s: String) { <b>this</b>.status = s }
 *    <b>def</b> getStatus: String = <b>this</b>.status
 *  </pre>
 *  <p>
 *    For fields of type <code>Boolean</code>, if you need a getter
 *    named <code>isStatus</code>, use the
 *    <code>scala.reflect.BooleanBeanProperty</code> annotation instead.
 *  </p>
 */
class BeanProperty extends StaticAnnotation
