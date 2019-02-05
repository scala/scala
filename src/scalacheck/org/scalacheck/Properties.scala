/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2017 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck

import org.scalacheck.rng.Seed

import language.reflectiveCalls

import util.ConsoleReporter

/** Represents a collection of properties, with convenient methods
 *  for checking all properties at once. This class is itself a property, which
 *  holds if and only if all of the contained properties hold.
 *  <p>Properties are added in the following way:</p>
 *
 *  {{{
 *  object MyProps extends Properties("MyProps") {
 *    property("myProp1") = forAll { (n:Int, m:Int) =>
 *      n+m == m+n
 *    }
 *  }
 *  }}}
 */
@Platform.EnableReflectiveInstantiation
class Properties(val name: String) {

  private val props = new scala.collection.mutable.ListBuffer[(String,Prop)]

  /**
   * Changes to the test parameters that are specific to this class.
   * Can be used to set custom parameter values for this test.
   */
  def overrideParameters(p: Test.Parameters): Test.Parameters = p

  /** Returns all properties of this collection in a list of name/property
   *  pairs.  */
  def properties: collection.Seq[(String,Prop)] = props

  /** Convenience method that checks the properties with the given parameters
   *  (or default parameters, if not specified)
   *  and reports the result on the console. Should only be used when running
   *  tests interactively within the Scala REPL.
   *
   *  If you need to get the results
   *  from the test use the `check` methods in [[org.scalacheck.Test]]
   *  instead. */
  def check(prms: Test.Parameters = Test.Parameters.default): Unit = {
    val params = overrideParameters(prms)
    Test.checkProperties(
      params.withTestCallback(ConsoleReporter(1) chain params.testCallback), this
    )
  }

  /** Convenience method that makes it possible to use this property collection
   *  as an application that checks itself on execution. Calls `System.exit`
   *  with the exit code set to the number of failed properties. */
  def main(args: Array[String]): Unit =
    Test.cmdLineParser.parseParams(args) match {
      case (applyCmdParams, Nil) =>
        val params = applyCmdParams(overrideParameters(Test.Parameters.default))
        val res = Test.checkProperties(params, this)
        val numFailed = res.count(!_._2.passed)
        if (numFailed > 0) {
          println(s"Found $numFailed failing properties.")
          System.exit(1)
        } else {
          System.exit(0)
        }
      case (_, os) =>
        println(s"Incorrect options: $os")
        Test.cmdLineParser.printHelp
        System.exit(-1)
    }

  /** Adds all properties from another property collection to this one */
  def include(ps: Properties): Unit =
    include(ps, prefix = "")

  /** Adds all properties from another property collection to this one
   *  with a prefix this is prepended to each included property's name. */
  def include(ps: Properties, prefix: String): Unit =
    for((n,p) <- ps.properties) property(prefix + n) = p

  /** Used for specifying properties. Usage:
   *  {{{
   *  property("myProp") = ...
   *  }}}
   */
  sealed class PropertySpecifier() {
    def update(propName: String, p: => Prop) = {
      props += ((name+"."+propName, Prop.delay(p)))
    }
  }

  lazy val property = new PropertySpecifier()

  sealed class PropertyWithSeedSpecifier() {
    def update(propName: String, optSeed: Option[String], p: => Prop) = {
      val fullName = s"$name.$propName"
      optSeed match {
        case Some(encodedSeed) =>
          val seed = Seed.fromBase64(encodedSeed).get
          props += ((fullName, Prop.delay(p).useSeed(fullName, seed)))
        case None =>
          props += ((fullName, Prop.delay(p).viewSeed(fullName)))
      }
    }
  }

  lazy val propertyWithSeed = new PropertyWithSeedSpecifier()
}
