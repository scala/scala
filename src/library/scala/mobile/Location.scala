/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.mobile


import java.lang.ClassLoader
import java.net._

import scala.collection.mutable._

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
  private var lcache: Map[URL, ClassLoader] = new HashMap

  /** The class loader associated with this location.
   */
  private val loader = if (url eq null)
    ClassLoader.getSystemClassLoader()
  else
    lcache.get(url) match {
    case Some(cl) =>
      cl
    case _ =>
      val cl = new URLClassLoader(Array(url))
      lcache(url) = cl
      cl
  }

  /** A cache containing all classes of this location.
   */
  private var ccache: Map[String, java.lang.Class[T] forSome { type T }] = new HashMap

  /** Return the code description for the string <code>className</code>
   *  at this location.
   *
   * @param classname the name of the class
   * @return          the code description corresponding to
   *                  <code>className</code>.
   */
  def create(className: String) = new Code(
    ccache.get(className) match {
      case Some(clazz) =>
        clazz
      case _ =>
        val clazz = if (loader.loadClass(className).isInterface()) {
          // Scala source: class A { ... };
          // Java bytecode: interface A.class + class A$class.class
          loader.loadClass(className + "$class")
        }
        else {
          // Scala source: object A { ... };
          // Java bytecode: interface A.class + class A$.class
          loader.loadClass(className + "$")
        }
        ccache(className) = clazz
        clazz
    }
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
