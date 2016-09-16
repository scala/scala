import sbt._
import Keys._

object Quiet {
  // Workaround SBT issue described:
  //
  //   https://github.com/scala/scala-dev/issues/100
  def silenceScalaBinaryVersionWarning = ivyConfiguration := {
    ivyConfiguration.value match {
      case c: InlineIvyConfiguration =>
        val delegate = c.log
        val logger = new Logger {
          override def trace(t: => Throwable): Unit = delegate.trace(t)
          override def log(level: sbt.Level.Value, message: => String): Unit = {
            level match {
              case sbt.Level.Warn =>
                val message0 = message
                val newLevel = if (message.contains("differs from Scala binary version in project"))
                  delegate.log(sbt.Level.Debug, message)
                else
                  delegate.log(level, message)
              case _ => delegate.log(level, message)
            }
          }
          override def success(message: => String): Unit = delegate.success(message)
        }
        new InlineIvyConfiguration(c.paths, c.resolvers, c.otherResolvers, c.moduleConfigurations, c.localOnly, c.lock, c.checksums, c.resolutionCacheDir, c.updateOptions, logger)
      case x => x
    }
  }
}
