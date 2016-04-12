/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package cmd

import nsc.io._
import java.util.Properties
import java.io.FileInputStream
import scala.sys.SystemProperties

/** Contains logic for translating a property key/value pair into
 *  equivalent command line arguments.  The default settings will
 *  translate, given programInfo.runner == "foo" :
 *
 *    foo.bar=true  to  --bar       // if --bar is unary
 *    foo.bar=quux  to  --bar quux  // if --bar is binary
 */
class PropertyMapper(reference: Reference) extends (((String, String)) => List[String]) {
  import reference._
  lazy val RunnerName = programInfo.runner

  // e.g. "partest.shootout" -> "--shootout"
  def propNameToOptionName(key: String): Option[String] = (key split '.').toList match {
    case List(RunnerName, name) => Some(name)
    case _                      => None
  }

  def isPassThrough(key: String): Boolean = false                 // e.g. "partest.options"
  def onError(key: String, value: String): Unit = ()              // called when translate fails

  def translate(key: String, value: String): List[String] = {
    val opt = toOpt(key)

    if (isUnaryOption(key) && isTrue(value)) List(opt)
    else if (isBinaryOption(key)) List(opt, value)
    else returning(Nil)(_ => onError(key, value))
  }
  def isTrue(value: String) = List("yes", "on", "true") contains value.toLowerCase

  def apply(kv: (String, String)): List[String] = {
    val (k, v) = kv

    if (isPassThrough(k)) toArgs(v)
    else propNameToOptionName(k) match {
      case Some(optName)  => translate(optName, v)
      case _              => Nil
    }
  }
}

trait Property extends Reference {
  def propMapper: PropertyMapper
  override def propertyArgs: List[String] = systemPropertiesToOptions

  def loadProperties(file: File): Properties  =
    returning(new Properties)(_ load new FileInputStream(file.path))

  def systemPropertiesToOptions: List[String] =
    propertiesToOptions(new SystemProperties().toList)

  def propertiesToOptions(file: File): List[String] =
    propertiesToOptions(loadProperties(file))

  def propertiesToOptions(props: java.util.Properties): List[String] = {
    import scala.collection.JavaConverters._
    propertiesToOptions(props.asScala.toList)
  }
  def propertiesToOptions(props: List[(String, String)]) = props flatMap propMapper
}
