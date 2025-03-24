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

package scala.demo

import scala.sys.process.Parser.tokenize
import scala.tools.nsc.io._
import scala.tools.partest.nest.{Reference, toOpt}
import scala.util.chaining._
import java.util.Properties
import java.io.FileInputStream

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
    else Nil.tap(_ => onError(key, value))
  }
  def isTrue(value: String) = List("yes", "on", "true") contains value.toLowerCase

  def apply(kv: (String, String)): List[String] = {
    val (k, v) = kv

    if (isPassThrough(k)) tokenize(v)
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
    new Properties().tap(_ load new FileInputStream(file.path))

  def systemPropertiesToOptions: List[String] =
    propertiesToOptions(System.getProperties)

  def propertiesToOptions(file: File): List[String] =
    propertiesToOptions(loadProperties(file))

  def propertiesToOptions(props: java.util.Properties): List[String] = {
    import scala.collection.JavaConverters._
    propertiesToOptions(props.asScala.toList)
  }
  def propertiesToOptions(props: List[(String, String)]) = props flatMap propMapper
}
