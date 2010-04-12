/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 */

package scala.tools
package partest

import nsc.io._
import nsc.util._
import category.AllCategories

/** Global object for a Partest run.  It is completely configured by the list
 *  of arguments passed to the constructor (although there are a few properties
 *  and environment variables which can influence matters.) See PartestSpec.scala
 *  for the complete list.
 */
class Partest(args: List[String]) extends {
  val parsed = PartestSpec(args: _*)
} with Universe with PartestSpec with cmd.Instance with AllCategories {

  if (parsed.propertyArgs.nonEmpty)
    debug("Partest property args: " + fromArgs(parsed.propertyArgs))

  debug("Partest created with args: " + fromArgs(args))

  def helpMsg     = PartestSpec.helpMsg

  // The abstract values from Universe.
  lazy val testBuildDir   = searchForDir(buildDir)
  lazy val partestDir     = searchForDir(rootDir)
  lazy val allCategories  = List(Pos, Neg, Run, Jvm, Res, Shootout, Scalap, Scalacheck, BuildManager, Script)

  lazy val selectedCategories = if (isAllImplied) allCategories else specifiedCats

  // Coarse validation of partest directory: holds a file called partest.
  (partestDir / "partest").isFile || error("'%s' is not a valid partest directory." format partestDir)

  def specifiedTests  = parsed.residualArgs map (x => Path(x).normalize)
  def specifiedKinds  = testKinds filter (x => isSet(x) || (runSets contains x))
  def specifiedCats   = specifiedKinds flatMap (x => allCategories find (_.kind == x))
  def isAllImplied    = isAll || (specifiedTests.isEmpty && specifiedKinds.isEmpty)

  /** Assembles a filter based on command line options which restrict the test set
   *  --grep limits to only matching tests
   *  --failed limits to only recently failed tests (log file is present)
   *  --<category> limits to only the given tests and categories (but --all overrides)
   *  path/to/Test limits to only the given tests and categories
   */
  lazy val filter = {
    def indivFilter(test: TestEntity)     = specifiedTests contains test.location.normalize
    def categoryFilter(test: TestEntity)  = specifiedCats contains test.category
    def indivOrCat(test: TestEntity)      = isAllImplied || indivFilter(test) || categoryFilter(test)  // combines previous two

    def failedFilter(test: TestEntity)    = !isFailed || (test.logFile exists)
    def grepFilter(test: TestEntity)      = grepExpr.isEmpty || (test containsString grepExpr.get)
    def combinedFilter(x: TestEntity)     = indivOrCat(x) && failedFilter(x) && grepFilter(x)   // combines previous three

    combinedFilter _
  }

  def launchTestSuite() = {
    def onTimeout() = {
      warning("Partest test run timed out after " + timeout + " seconds.\n")
      System.exit(-1)
    }
    val alarm = new Alarmer(AlarmerAction(timeout, () => onTimeout()))

    try runSelection(selectedCategories, filter)
    finally alarm.cancelAll()
  }
}

object Partest {
  def fromBuild(dir: String, args: String*): Partest      = apply("--builddir" +: dir +: args: _*)
  def apply(args: String*): Partest                       = new Partest(args.toList)

  // builds without partest jars won't actually work
  def starr()   = fromBuild("")
  def locker()  = fromBuild("build/locker")
  def quick()   = fromBuild("build/quick")
  def pack()    = fromBuild("build/pack")
  def strap()   = fromBuild("build/strap")
  def dist()    = fromBuild("dists/latest")
}

