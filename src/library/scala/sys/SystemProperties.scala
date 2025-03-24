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

package scala
package sys

import scala.collection.{mutable, Iterator}
import scala.jdk.CollectionConverters._
import java.security.AccessControlException
import scala.language.implicitConversions

/** A bidirectional map wrapping the java System properties.
 *  Changes to System properties will be immediately visible in the map,
 *  and modifications made to the map will be immediately applied to the
 *  System properties.  If a security manager is in place which prevents
 *  the properties from being read or written, the AccessControlException
 *  will be caught and discarded.
 *  @define Coll `collection.mutable.Map`
 *  @define coll mutable map
 */
class SystemProperties
extends mutable.AbstractMap[String, String] {

  override def empty: mutable.Map[String, String] = mutable.Map[String, String]()
  override def default(key: String): String = null

  def iterator: Iterator[(String, String)] = wrapAccess {
    val ps = System.getProperties()
    names map (k => (k, ps getProperty k)) filter (_._2 ne null)
  } getOrElse Iterator.empty

  override def isEmpty: Boolean = iterator.isEmpty
  def names: Iterator[String] = wrapAccess (
    System.getProperties().stringPropertyNames().asScala.iterator
  ) getOrElse Iterator.empty

  def get(key: String): Option[String] =
    wrapAccess(Option(System.getProperty(key))) flatMap (x => x)
  override def contains(key: String): Boolean =
    wrapAccess(super.contains(key)) exists (x => x)

  override def clear(): Unit = wrapAccess(System.getProperties().clear())
  def subtractOne (key: String): this.type = { wrapAccess(System.clearProperty(key)) ; this }
  def addOne (kv: (String, String)): this.type = { wrapAccess(System.setProperty(kv._1, kv._2)) ; this }

  @annotation.nowarn("cat=deprecation") // AccessControlException is deprecated on JDK 17
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
  /** An unenforceable, advisory only place to do some synchronization when
   *  mutating system properties.
   */
  def exclusively[T](body: => T): T = this synchronized body

  implicit def systemPropertiesToCompanion(p: SystemProperties): SystemProperties.type = this

  private final val HeadlessKey            = "java.awt.headless"
  private final val PreferIPv4StackKey     = "java.net.preferIPv4Stack"
  private final val PreferIPv6AddressesKey = "java.net.preferIPv6Addresses"
  private final val NoTraceSuppressionKey  = "scala.control.noTraceSuppression"

  def help(key: String): String = key match {
    case HeadlessKey            => "system should not utilize a display device"
    case PreferIPv4StackKey     => "system should prefer IPv4 sockets"
    case PreferIPv6AddressesKey => "system should prefer IPv6 addresses"
    case NoTraceSuppressionKey  => "scala should not suppress any stack trace creation"
    case _                      => ""
  }

  lazy val headless: BooleanProp            = BooleanProp.keyExists(HeadlessKey)
  lazy val preferIPv4Stack: BooleanProp     = BooleanProp.keyExists(PreferIPv4StackKey)
  lazy val preferIPv6Addresses: BooleanProp = BooleanProp.keyExists(PreferIPv6AddressesKey)
  lazy val noTraceSuppression: BooleanProp  = BooleanProp.valueIsTrue(NoTraceSuppressionKey)
}

