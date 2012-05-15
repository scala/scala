/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package settings

/** Taking flag checking to a somewhat higher level. */
trait AestheticSettings {
  def settings: Settings

  // Some(value) if setting has been set by user, None otherwise.
  def optSetting[T](s: Settings#Setting): Option[T] =
    if (s.isDefault) None else Some(s.value.asInstanceOf[T])

  def script       = optSetting[String](settings.script)
  def encoding     = optSetting[String](settings.encoding)
  def sourceReader = optSetting[String](settings.sourceReader)

  def debug           = settings.debug.value
  def declsOnly       = false
  def deprecation     = settings.deprecation.value
  def experimental    = settings.Xexperimental.value
  def fatalWarnings   = settings.fatalWarnings.value
  def feature         = settings.feature.value
  def future          = settings.future.value
  def logClasspath    = settings.Ylogcp.value
  def printStats      = settings.Ystatistics.value
  def target          = settings.target.value
  def unchecked       = settings.unchecked.value
  def verbose         = settings.verbose.value
  def virtPatmat      = !settings.XoldPatmat.value

  /** Derived values */
  def jvm           = target startsWith "jvm"
  def msil          = target == "msil"
  def verboseDebug  = debug && verbose
}
