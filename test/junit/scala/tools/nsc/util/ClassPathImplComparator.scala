/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.util

import scala.reflect.io.AbstractFile
import scala.tools.nsc.Settings
import scala.tools.nsc.settings.ClassPathImplementationType
import scala.tools.util.PathResolverFactory

/**
 * Simple application to compare efficiency of the old and the new classpath implementation
 */
object ClassPathImplComparator {

  private class TestSettings extends Settings {
    val checkClasses = PathSetting("-checkClasses", "Specify names of classes which should be found", "")
    val requiredIterations = IntSetting("-requiredIterations",
      "Repeat tests specified number of times (to check e.g. impact of caches)", 1, Some((1, Int.MaxValue)), (_: String) => None)
  }

  private class DurationStats(name: String) {
    private var sum = 0L
    private var iterations = 0

    def noteMeasuredTime(millis: Long): Unit = {
      sum += millis
      iterations += 1
    }

    def printResults(): Unit = {
      val avg = if (iterations == 0) 0 else sum.toDouble / iterations
      println(s"$name - total duration: $sum ms; iterations: $iterations; avg: $avg ms")
    }
  }

  private lazy val defaultClassesToFind = List(
    "scala.collection.immutable.List",
    "scala.Option",
    "scala.Int",
    "scala.collection.immutable.Vector",
    "scala.util.hashing.MurmurHash3"
  )

  private val oldCpCreationStats = new DurationStats("Old classpath - create")
  private val oldCpSearchingStats = new DurationStats("Old classpath - search")

  private val flatCpCreationStats = new DurationStats("Flat classpath - create")
  private val flatCpSearchingStats = new DurationStats("Flat classpath - search")

  def main(args: Array[String]): Unit = {

    if (args contains "-help")
      usage()
    else {
      val oldCpSettings = loadSettings(args.toList, ClassPathImplementationType.Recursive)
      val flatCpSettings = loadSettings(args.toList, ClassPathImplementationType.Flat)

      val classesToCheck = oldCpSettings.checkClasses.value
      val classesToFind =
        if (classesToCheck.isEmpty) defaultClassesToFind
        else classesToCheck.split(";").toList

      def doTest(classPath: => ClassFileLookup[AbstractFile])
                (cpCreationStats: DurationStats, cpSearchingStats: DurationStats) = {
        val cp = withMeasuredTime("Creating classpath", classPath, cpCreationStats)
        val result = withMeasuredTime("Searching for specified classes", checkExistenceOfClasses(classesToFind)(cp), cpSearchingStats)
        println(s"The end of test case. All expected classes found = $result \n")
      }

      (1 to oldCpSettings.requiredIterations.value) foreach { iteration =>
        if (oldCpSettings.requiredIterations.value > 1)
          println(s"Iteration no $iteration")

        println("Old classpath implementation:")
        doTest(PathResolverFactory.create(oldCpSettings).result)(oldCpCreationStats, oldCpSearchingStats)

        println("Flat classpath implementation:")
        doTest(PathResolverFactory.create(flatCpSettings).result)(flatCpCreationStats, flatCpSearchingStats)
      }

      if (oldCpSettings.requiredIterations.value > 1) {
        println("\nOld classpath - summary")
        oldCpCreationStats.printResults()
        oldCpSearchingStats.printResults()

        println("\nFlat classpath - summary")
        flatCpCreationStats.printResults()
        flatCpSearchingStats.printResults()
      }
    }
  }

  /**
   * Prints usage information
   */
  private def usage(): Unit =
    println("""Use classpath and sourcepath options like in the case of e.g. 'scala' command.
              | There are also two additional options:
              | -checkClasses <semicolon separated class names> Specify names of classes which should be found
              | -requiredIterations <int value>                 Repeat tests specified count of times (to check e.g. impact of caches)
              | Note: Option -YclasspathImpl will be set automatically for each case.
            """.stripMargin.trim)

  private def loadSettings(args: List[String], implType: String) = {
    val settings = new TestSettings()
    settings.processArguments(args, true)
    settings.YclasspathImpl.value = implType
    if (settings.classpath.isDefault)
      settings.classpath.value = sys.props("java.class.path")
    settings
  }

  private def withMeasuredTime[T](operationName: String, f: => T, durationStats: DurationStats): T = {
    val startTime = System.currentTimeMillis()
    val res = f
    val elapsed = System.currentTimeMillis() - startTime
    durationStats.noteMeasuredTime(elapsed)
    println(s"$operationName - elapsed $elapsed ms")
    res
  }

  private def checkExistenceOfClasses(classesToCheck: Seq[String])(classPath: ClassFileLookup[AbstractFile]): Boolean =
    classesToCheck.foldLeft(true) {
      case (res, classToCheck) =>
        val found = classPath.findClass(classToCheck).isDefined
        if (!found)
          println(s"Class $classToCheck not found") // of coure in this case measured time will be affected by IO operation
        found
    }
}
