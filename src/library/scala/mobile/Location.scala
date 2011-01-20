/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.mobile

import java.net._
import scala.collection.mutable

/** The class <code>Location</code> provides a <code>create</code>
 *  method to instantiate objects from a network location by
 *  specifying the URL address of the jar/class file.<p/>
 *
 *  An update of the jar/class file should not break your code as far
 *  as the used class names and method signatures are the same.<p/>
 *
 *  Example:<pre>
 *    <b>val</b> url = <b>new</b> URL("http://scala-lang.org/classes/examples.jar");
 *    <b>val</b> obj = <b>new</b> Location(url) create "examples.sort";</pre>
 *
 *  @see <a href="Code.html">Code</a>
 *
 *  @author  Stephane Micheloud
 *  @version 1.0, 04/05/2004
 */
class Location(url: URL) {
  /** A cache containing all class loaders of this location.
   */
  private val lcache = new mutable.HashMap[URL, ClassLoader]

  /** The class loader associated with this location.
   */
  private val loader =
    if (url eq null) ClassLoader.getSystemClassLoader()
    else lcache.getOrElseUpdate(url, new URLClassLoader(Array(url)))

  /** A cache containing all classes of this location.
   */
  private val ccache = new mutable.HashMap[String, java.lang.Class[_]]

  /** Return the code description for the string <code>className</code>
   *  at this location.
   *
   * @param classname the name of the class
   * @return          the code description corresponding to `className`.
   */
  def create(className: String) = new Code(
    ccache.getOrElseUpdate(className, {
      // source 'class A { ... }' becomes in bytecode: interface A.class + class A$class.class
      // source 'object A { ... }' becomes in bytecode: interface A.class + class A$.class
      val append = if (loader.loadClass(className).isInterface) "$class" else "$"
      loader loadClass (className + append)
    })
  )
}

/** The object <code>Location</code> can be used to instantiate
 *  objects on the same Java VM. It is just provided to illustrate
 *  the special case where resources are available locally.<p/>
 *
 *  Example:<pre>
 *    <b>val</b> obj = Location.create("xcode.Math");
 *    <b>val</b> x = obj[Int, Int]("square")(5);</pre>
 *
 *  @author  Stephane Micheloud
 *  @version 1.0, 04/05/2004
 */
object Location extends Location(null)
