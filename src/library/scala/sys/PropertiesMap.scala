/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.sys

import scala.collection.mutable
import scala.collection.JavaConverters._

/** A bidirectional map wrapping the java System properties.
 *  Changes to System properties will be immediately visible in the map,
 *  and modifications made to the map will be immediately applied to the
 *  System properties.
 *
 *  @author Paul Phillips
 *  @version 2.9
 *  @since   2.9
 */
class PropertiesMap extends mutable.Map[String, String] {
  override def empty = new PropertiesMap
  override def default(key: String): String = null
  def iterator: Iterator[(String, String)] = System.getProperties().asScala.iterator
  def get(key: String) = Option(System.getProperty(key))

  def -= (key: String): this.type = { System.clearProperty(key) ; this }
  def += (kv: (String, String)): this.type = { System.setProperty(kv._1, kv._2) ; this }
}