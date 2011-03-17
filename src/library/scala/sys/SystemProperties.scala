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
import java.security.AccessControlException

/** A bidirectional map wrapping the java System properties.
 *  Changes to System properties will be immediately visible in the map,
 *  and modifications made to the map will be immediately applied to the
 *  System properties.  If a security manager is in place which prevents
 *  the properties from being read or written, the AccessControlException
 *  will be caught and discarded.
 *
 *  @author Paul Phillips
 *  @version 2.9
 *  @since   2.9
 */
class SystemProperties extends mutable.Map[String, String] {
  override def empty = new SystemProperties
  override def default(key: String): String = null

  def iterator: Iterator[(String, String)] =
    wrapAccess(System.getProperties().asScala.iterator) getOrElse Iterator.empty
  def get(key: String) =
    wrapAccess(Option(System.getProperty(key))) flatMap (x => x)
  override def contains(key: String) =
    wrapAccess(super.contains(key)) exists (x => x)

  def -= (key: String): this.type = { wrapAccess(System.clearProperty(key)) ; this }
  def += (kv: (String, String)): this.type = { wrapAccess(System.setProperty(kv._1, kv._2)) ; this }

  def wrapAccess[T](body: => T): Option[T] =
    try Some(body) catch { case _: AccessControlException => None }
}

/** The values in SystemProperties can be used to access and manipulate
 *  designated system properties.  See `scala.sys.Prop` for particulars.
 *  @example {{{
 *    if (!headless.isSet) headless.enable()
 *  }}}
 */
object SystemProperties {
  implicit def systemPropertiesToCompanion(p: SystemProperties): SystemProperties.type = this
  private lazy val propertyHelp = mutable.Map[String, String]()
  private def bool(key: String, helpText: String) = {
    val prop = (
      if (key startsWith "java.") BooleanProp.valueIsTrue(key)
      else BooleanProp.keyExists(key)
    )
    propertyHelp(key) = helpText
    prop
  }
  def help(key: String) = propertyHelp.getOrElse(key, "")

  // Todo: bring some sanity to the intersection of system properties aka "mutable
  // state shared by everyone and everything" and the reality that there is no other
  // mechanism for accomplishing some things on the jvm.
  lazy val headless          = bool("java.awt.headless", "system should not utilize a display device")
  lazy val preferIPv4        = bool("java.net.preferIPv4Stack", "system should prefer IPv4 sockets")
  lazy val noTraceSupression = bool("scala.control.no-trace-suppression", "scala should not suppress any stack trace creation")
}

