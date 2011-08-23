/* NSC -- new Scala compiler
 * Copyright 2006-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package reflect

import scala.util.PropertiesTrait
import java.security.AccessControlException

/** For placing a wrapper function around property functions.
 *  Motivated by places like google app engine throwing exceptions
 *  on property lookups.
 */
trait WrappedProperties extends PropertiesTrait {
  def wrap[T](body: => T): Option[T]

  protected def propCategory   = "wrapped"
  protected def pickJarBasedOn = this.getClass

  override def propIsSet(name: String)               = wrap(super.propIsSet(name)) exists (x => x)
  override def propOrElse(name: String, alt: String) = wrap(super.propOrElse(name, alt)) getOrElse alt
  override def setProp(name: String, value: String)  = wrap(super.setProp(name, value)) orNull
  override def clearProp(name: String)               = wrap(super.clearProp(name)) orNull
  override def envOrElse(name: String, alt: String)  = wrap(super.envOrElse(name, alt)) getOrElse alt
  override def envOrNone(name: String)               = wrap(super.envOrNone(name)).flatten

  def systemProperties: Iterator[(String, String)] = {
    import scala.collection.JavaConverters._
    wrap(System.getProperties.asScala.iterator) getOrElse Iterator.empty
  }
}

object WrappedProperties {
  object AccessControl extends WrappedProperties {
    def wrap[T](body: => T) = try Some(body) catch { case _: AccessControlException => None }
  }
}
