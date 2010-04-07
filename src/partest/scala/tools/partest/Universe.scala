/*                     __                                               *\
**     ________ ___   / /  ___     Scala Parallel Testing               **
**    / __/ __// _ | / /  / _ |    (c) 2007-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.tools
package partest

import nsc.io._
import category.AllCategories
import io.Logging

/** The high level view of the partest infrastructure.
 */
abstract class Universe
      extends Entities
      with BuildContributors
      with Logging
      with Dispatcher
      with Statistics
      with Housekeeping
      with Results
      with PartestCompilation
      with PartestSpec
      with Config
      with Alarms
      with Actions
      with Categories {

  /** The abstract values from which all else is derived. */
  def partestDir: Directory
  def testBuildDir: Directory
  def allCategories: List[TestCategory]
  def selectedCategories: List[TestCategory]

  /** Some plausibly abstract types. */
  type TestBuild    <: BuildContributor   // e.g. quick, pack
  type TestCategory <: AbsTestCategory    // e.g. pos, neg, run
  type TestEntity   <: AbsTestEntity      // e.g. files/pos/test25.scala
  type TestSequence <: AbsTestSequence    // e.g. compile, run, diff

  /** Although TestStep isn't much more than Function1 right now,
   *  it exists this way so it can become more capable.
   */
  implicit def f1ToTestStep(f: TestEntity => Boolean): TestStep =
    new TestStep { def apply(test: TestEntity) = f(test) }

  abstract class TestStep extends (TestEntity => Boolean) {
    def apply(test: TestEntity): Boolean
  }

  /** An umbrella category of tests, such as "pos" or "run".
   */
  trait AbsTestCategory extends BuildContributor {
    type TestSettings

    def kind: String
    def testSequence: TestSequence
    def denotesTest(location: Path): Boolean

    def createTest(location: Path): TestEntity
    def createSettings(entity: TestEntity): TestSettings
    def enumerate: List[TestEntity]
  }

  /** A single test.  It may involve multiple files, but only a
   *  single path is used to designate it.
   */
  trait AbsTestEntity extends BuildContributor {
    def category: TestCategory
    def location: Path
    def onException(x: Throwable): Unit
    def testClasspath: String

    /** Any preconditions before running the test.  Test fails
     *  immediately if this returns false.
     */
    def precondition: Boolean = true

    /** Most tests will use the sequence defined by the category,
     *  but the test can override and define a custom sequence.
     */
    def testSequence: TestSequence

    /** True if this test recognizes the given path as a piece of it.
     *  For validation purposes.
     */
    def acknowledges(path: Path): Boolean
  }

  /** Every TestEntity is partly characterized by a series of actions
   *  which are applied to the TestEntity in the given order.  The test
   *  passes if all those actions return true, fails otherwise.
   */
  trait AbsTestSequence {
    def actions: List[TestStep]
  }
}