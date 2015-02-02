/*
 * Copyright (c) 2014 Contributor. All rights reserved.
 */
package scala.tools.nsc

import scala.io.StdIn.readLine

/**
 * Simple application to check out amount of memory used by chosen classpath representation.
 * It allows us to create many scalac-like calls based on specified parameters, where each main retains Global.
 * And we need additional tool (e.g. profiler) to measure memory consumption itself.
 */
object ClassPathMemoryConsumptionTester {

  private class TestSettings extends Settings {
    val requiredInstances = IntSetting("-requiredInstances",
      "Determine how many times classpath should be loaded", 10, Some((1, 10000)), (_: String) => None)
  }

  private class MainRetainsGlobal extends scala.tools.nsc.MainClass {
    var retainedGlobal: Global = _
    override def doCompile(compiler: Global) {
      retainedGlobal = compiler
      super.doCompile(compiler)
    }
  }

  def main(args: Array[String]): Unit = {
    if (args contains "-help") usage()
    else doTest(args)
  }

  private def doTest(args: Array[String]) = {
    val settings = loadSettings(args.toList)

    val mains = (1 to settings.requiredInstances.value) map (_ => new MainRetainsGlobal)

    // we need original settings without additional params to be able to use them later
    val baseArgs = argsWithoutRequiredInstances(args)

    println(s"Loading classpath ${settings.requiredInstances.value} times")
    val startTime = System.currentTimeMillis()

    mains map (_.process(baseArgs))

    val elapsed = System.currentTimeMillis() - startTime
    println(s"Operation finished - elapsed $elapsed ms")
    println("Memory consumption can be now measured")

    var textFromStdIn = ""
    while (textFromStdIn.toLowerCase != "exit")
      textFromStdIn = readLine("Type 'exit' to close application: ")
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
    settings.processArguments(args, processAll = true)
    if (settings.classpath.isDefault)
      settings.classpath.value = sys.props("java.class.path")
    settings
  }

  private def argsWithoutRequiredInstances(args: Array[String]) = {
    val instancesIndex = args.indexOf("-requiredInstances")
    if (instancesIndex == -1) args
    else args.dropRight(args.length - instancesIndex) ++ args.drop(instancesIndex + 2)
  }
}
