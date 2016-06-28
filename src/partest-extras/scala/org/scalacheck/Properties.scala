/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2014 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck

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
class Properties(val name: String) extends Prop {

  private val props = new scala.collection.mutable.ListBuffer[(String,Prop)]

  /** Returns one property which holds if and only if all of the
   *  properties in this property collection hold */
  private def oneProperty: Prop = Prop.all((properties map (_._2)):_*)

  /** Returns all properties of this collection in a list of name/property
   *  pairs.  */
  def properties: Seq[(String,Prop)] = props

  def apply(p: Gen.Parameters) = oneProperty(p)

  /** Convenience method that checks the properties with the given parameters
   *  and reports the result on the console. If you need to get the results
   *  from the test use the `check` methods in [[org.scalacheck.Test]]
   *  instead. */
  override def check(prms: Test.Parameters): Unit = Test.checkProperties(
    prms.withTestCallback(ConsoleReporter(1) chain prms.testCallback), this
  )

  /** Convenience method that checks the properties and reports the
   *  result on the console. If you need to get the results from the test use
   *  the `check` methods in [[org.scalacheck.Test]] instead. */
  override def check: Unit = check(Test.Parameters.default)

  /** The logic for main, separated out to make it easier to
   *  avoid System.exit calls.  Returns exit code.
   */
  override def mainRunner(args: Array[String]): Int = {
    Test.parseParams(args) match {
      case Some(params) =>
        val res = Test.checkProperties(params, this)
        val failed = res.filter(!_._2.passed).size
        failed
      case None =>
        println("Incorrect options")
        -1
    }
  }

  /** Adds all properties from another property collection to this one. */
  def include(ps: Properties) = for((n,p) <- ps.properties) property(n) = p

  /** Used for specifying properties. Usage:
   *  {{{
   *  property("myProp") = ...
   *  }}}
   */
  class PropertySpecifier() {
    def update(propName: String, p: Prop) = props += ((name+"."+propName, p))
  }

  lazy val property = new PropertySpecifier()
}
