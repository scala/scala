/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2011 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck

/** Represents a collection of properties, with convenient methods
 *  for checking all properties at once. This class is itself a property, which
 *  holds if and only if all of the contained properties hold.
 *  <p>Properties are added in the following way:</p>
 *
 *  <p>
 *  <code>
 *  object MyProps extends Properties("MyProps") {
 *    property("myProp1") = forAll { (n:Int, m:Int) =&gt;
 *      n+m == m+n
 *    }
 *
 *    property("myProp2") = ((0/1) throws classOf[ArithmeticException])
 *  }
 */
class Properties(val name: String) extends Prop {

  import Test.cmdLineParser.{Success, NoSuccess}

  private val props = new scala.collection.mutable.ListBuffer[(String,Prop)]

  /** Returns one property which holds if and only if all of the
   *  properties in this property collection hold */
  private def oneProperty: Prop = Prop.all((properties map (_._2)):_*)

  /** Returns all properties of this collection in a list of name/property
   *  pairs.  */
  def properties: Seq[(String,Prop)] = props

  def apply(p: Prop.Params) = oneProperty(p)

  /** Convenience method that checks the properties with the given parameters
   *  and reports the result on the console. If you need to get the results
   *  from the test use the <code>check</code> methods in <code>Test</code>
   *  instead. */
  override def check(prms: Test.Params): Unit = Test.checkProperties(
    prms copy (testCallback = ConsoleReporter(1) chain prms.testCallback), this
  )

  /** Convenience method that checks the properties and reports the
   *  result on the console. If you need to get the results from the test use
   *  the <code>check</code> methods in <code>Test</code> instead. */
  override def check: Unit = check(Test.Params())

  /** The logic for main, separated out to make it easier to
   *  avoid System.exit calls.  Returns exit code.
   */
  override def mainRunner(args: Array[String]): Int = {
    Test.cmdLineParser.parseParams(args) match {
      case Success(params, _) =>
        val res = Test.checkProperties(params, this)
        val failed = res.filter(!_._2.passed).size
        failed
      case e: NoSuccess =>
        println("Incorrect options:"+"\n"+e+"\n")
        Test.cmdLineParser.printHelp
        -1
    }
  }

  /** Adds all properties from another property collection to this one. */
  def include(ps: Properties) = for((n,p) <- ps.properties) property(n) = p

  /** Used for specifying properties. Usage:
   *  <code>property("myProp") = ...</code> */
  class PropertySpecifier() {
    def update(propName: String, p: Prop) = props += ((name+"."+propName, p))
  }

  lazy val property = new PropertySpecifier()
}
