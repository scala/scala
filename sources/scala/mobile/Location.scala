/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.mobile;

import java.net._;

import scala.collection.mutable._;


/** The class <code>Location</code> provides a <code>create</code>
 *  method to instanciate objects from a network location by
 *  specifying its URL address.
 *
 *  Example:<pre>
 *    val url = new URL("http://scala.epfl.ch/classes/examples.jar");
 *    val obj = new Location(url) create "examples.sort";</pre>
 *
 *  @author  Stephane Micheloud
 *  @version 1.0, 04/05/2004
 */
class Location(url: URL) {

  /** A cache containing all class loaders of this location.
   */
  private var lcache: Map[URL, ClassLoader] = new HashMap;

  /** The class loader associated with this location.
   */
  private val loader = if (url == null)
    ClassLoader.getSystemClassLoader()
  else
    lcache.get(url) match {
    case Some(cl) =>
      cl
    case _ =>
      val cl = new URLClassLoader(Predef.Array(url));
      lcache(url) = cl;
      cl
  };

  /** A cache containing all classes of this location.
   */
  private var ccache: Map[String, java.lang.Class] = new HashMap;

  def create(className: String): Code = {
    val clazz = ccache.get(className) match {
      case Some(x) =>
        x
      case _ =>
        val clazz = if (loader.loadClass(className).isInterface()) {
          // Scala source: class A { ... };
          // Java bytecode: interface A.class + class A$class.class
          loader.loadClass(className + "$class");
        }
        else {
          // Scala source: object A { ... };
          // Java bytecode: interface A.class + class A$.class
          loader.loadClass(className + "$");
        }
        ccache(className) = clazz;
        clazz
    };
    new Code(clazz)
  }

}

object Location extends Location(null);
