/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
*/

// $Id$


package scala.reflect

/**
 * This attribute adds a setter and a getter method, following the
 Java Bean convention (first letter of the property is capitalized) used
by popular Java web frameworks.
For example
<pre>
  [BeanProperty]
  var status = ""
</pre>
<p> adds the following methods to the <b>generated</b> code </p>
<pre>
  def setStatus(s:String): Unit = { this.status = s }
  def getStatus: String         = { this.status }
</pre>
 *
 <p>
 However, you cannot call <code>setStatus</code> from Scala, you should
 use the normal Scala access and assignment.
 </p>
 */
class BeanProperty extends Annotation
