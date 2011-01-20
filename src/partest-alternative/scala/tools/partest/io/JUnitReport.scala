/* NEST (New Scala Test)
 * Copyright 2007-2011 LAMP/EPFL
 */

package scala.tools
package partest
package io

/** This is disabled for the moment but I can fix it up if anyone
 *  is using it.
 */
class JUnitReport {
  // create JUnit Report xml files if directory was specified
  // def junitReport(dir: Directory) = {
  //   dir.mkdir()
  //   val report = testReport(set.kind, results, succs, fails)
  //   XML.save("%s/%s.xml".format(d.toAbsolute.path, set.kind), report)
  // }

  // def oneResult(res: (TestEntity, Int)) =
  //   <testcase name={res._1.path}>{
  //    res._2 match {
  //      case 0 => scala.xml.NodeSeq.Empty
  //       case 1 => <failure message="Test failed"/>
  //       case 2 => <failure message="Test timed out"/>
  //    }
  //  }</testcase>
  //
  // def testReport(kind: String, results: Iterable[(TestEntity, Int)], succs: Int, fails: Int) = {
  //   <testsuite name={kind} tests={(succs + fails).toString} failures={fails.toString}>
  //    <properties/>
  //    {
  //      results.map(oneResult(_))
  //    }
  //   </testsuite>
  // }
  //
}