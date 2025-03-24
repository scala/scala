/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools
package reflect

import scala.util.PropertiesTrait
import java.security.AccessControlException

/** For placing a wrapper function around property functions.
 *  Motivated by places like Google App Engine throwing exceptions
 *  on property lookups.
 */
trait WrappedProperties extends PropertiesTrait {
  def wrap[T](body: => T): Option[T]

  protected def propCategory   = "wrapped"
  protected def pickJarBasedOn = this.getClass

  override def propIsSet(name: String)               = wrap(super.propIsSet(name)) exists (x => x)
  override def propOrElse(name: String, alt: => String) = wrap(super.propOrElse(name, alt)) getOrElse alt
  override def setProp(name: String, value: String)  = wrap(super.setProp(name, value)).orNull
  override def clearProp(name: String)               = wrap(super.clearProp(name)).orNull
  override def envOrElse(name: String, alt: => String)  = wrap(super.envOrElse(name, alt)) getOrElse alt
  override def envOrNone(name: String)               = wrap(super.envOrNone(name)).flatten
  override def envOrSome(name: String, alt: => Option[String]) = wrap(super.envOrNone(name)).flatten orElse alt

  def systemProperties: List[(String, String)] = {
    import scala.jdk.CollectionConverters._
    wrap {
      // scala/bug#7269,7775 Avoid `ConcurrentModificationException` and nulls if another thread modifies properties
      val props = System.getProperties
      val it = props.stringPropertyNames().asScala.iterator map (k => (k, props getProperty k)) filter (_._2 ne null)
      it.toList
    } getOrElse Nil
  }
}

object WrappedProperties {
  object AccessControl extends WrappedProperties {
    @annotation.nowarn("cat=deprecation") // AccessControlException is deprecated on JDK 17
    def wrap[T](body: => T) = try Some(body) catch { case _: AccessControlException => None }
  }
}
