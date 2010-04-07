/* NEST (New Scala Test)
 * Copyright 2007-2010 LAMP/EPFL
 * @author Philipp Haller
 */

package scala.tools
package partest

import nsc.io._

object Runner {
  def main(mainArgs: Array[String]) {
    val propArgs = PartestSpecReference.sysPropsAsOptions()
    val args = (propArgs ++ mainArgs).toList
    val runner = Partest(args: _*)
    import runner._

    if (isVersion) return println(versionMsg)
    if (args.isEmpty) return println(helpMsg)
    if (isValidate) return validateAll()

    printConfigBanner()

    if (isCleanup)
      cleanupAll()

    val result    = launchTestSuite()
    val exitCode  = result.exitCode
    val message   = "\n" + result + "\n"

    if (exitCode == 0) success(message)
    else failure(message)

    if (isStats)
      showTestStatistics()

    System exit exitCode
  }
}
