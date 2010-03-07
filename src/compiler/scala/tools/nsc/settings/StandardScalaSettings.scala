/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package settings

/** Settings which aren't behind a -X, -Y, or -P option.
 *  Wherever possible, the val and the option have identical.
 *  names.
 */
trait StandardScalaSettings {
  self: AbsScalacSettings =>

  /** Path related settings.
   */
  val bootclasspath: PathSetting
  val classpath: PathSetting
  val d: OutputSetting
  val extdirs: PathSetting
  val g: DebugSetting
  val javabootclasspath: PathSetting
  val javaextdirs: PathSetting
  val javaignorecp: BooleanSetting
  val sourcepath: StringSetting

  /** Other settings.
   */
  val dependencyfile: StringSetting
  val deprecation: BooleanSetting
  val encoding: StringSetting
  val explaintypes: BooleanSetting
  val help: BooleanSetting
  val make: ChoiceSetting
  val nowarn: BooleanSetting
  val optimise: BooleanSetting
  val print: BooleanSetting
  val target: ChoiceSetting
  val unchecked: BooleanSetting
  val uniqid: BooleanSetting
  val verbose: BooleanSetting
  val version: BooleanSetting

  /** These are @<file> and -Dkey=val style settings, which don't
   *  nicely map to identifiers.
   */
  val argfiles: BooleanSetting
  val defines: DefinesSetting

  /** Compatibility stubs for options whose value name did
   *  not previously match the option name.
   */
  def XO = optimise
  def debuginfo = g
  def dependenciesFile = dependencyfile
  def nowarnings = nowarn
  def outdir = d
  def printLate = print
}
