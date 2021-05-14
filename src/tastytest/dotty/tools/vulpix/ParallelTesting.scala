package dotty.tools.vulpix

/** As of Scala 3.0.0-RC2, dotty compiler will enable the
 *  usage of experimental features if the compiler is invoked
 *  within a method on the class `dotty.tools.vulpix.ParallelTesting`
 *
 *  We use this to test experimental features on non-nightly releases.
 */
class ParallelTesting {
  def unlockExperimentalFeatures[T](op: => T): T = op
}
