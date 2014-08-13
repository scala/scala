/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc.util

import scala.io.StdIn.readLine
import scala.tools.nsc.Settings
import scala.tools.util.PathResolverFactory

/**
 * Simple application to check out amount of memory used by chosen classpath implementation.
 * It allows us to create many classpath instances based on specified parameters.
 * And we need additional tool (e.g. profiler) to measure memory consumption itself.
 */
object ClassPathMemoryConsumptionTester {

  private class TestSettings extends Settings {
    val requiredInstances = IntSetting("-requiredInstances",
      "Determine how many times classpath should be loaded", 10, Some((1, 10000)), (_: String) => None)
  }

  def main(args: Array[String]): Unit = {

    if (args contains "-help") usage()
    else doTest(args)
  }

  private def doTest(args: Array[String]) = {
    val settings = loadSettings(args.toList)

    println(s"Loading classpath ${settings.requiredInstances.value} times")
    val startTime = System.currentTimeMillis()

    val tmp = (1 to settings.requiredInstances.value) map (i => PathResolverFactory.create(settings).result)

    val elapsed = System.currentTimeMillis() - startTime
    println(s"Operation finished - elapsed $elapsed ms")
    println("Memory consumption can be now measured")

    var textFromStdIn = ""
    while (textFromStdIn.toLowerCase != "exit")
      textFromStdIn = readLine("Type 'exit' to close application: ")

    tmp // just to skip warning about unused variable
  }

  /**
   * Prints usage information
   */
  private def usage(): Unit =
    println( """Use classpath and sourcepath options like in the case of e.g. 'scala' command.
               | There's also one additional option:
               | -requiredInstances <int value> Determine how many times classpath should be loaded
             """.stripMargin.trim)

  private def loadSettings(args: List[String]) = {
    val settings = new TestSettings()
    settings.processArguments(args, true)
    if (settings.classpath.isDefault)
      settings.classpath.value = sys.props("java.class.path")
    settings
  }
}
