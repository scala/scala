/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package settings

trait AbsScalaSettings {
  self: AbsSettings =>

  type Setting <: AbsSetting

  type BooleanSetting     <: Setting { type T = Boolean }
  type ChoiceSetting      <: Setting { type T = String }
  type IntSetting         <: Setting { type T = Int }
  type MultiStringSetting <: Setting { type T = List[String] }
  type PathSetting        <: Setting { type T = String }
  type PhasesSetting      <: Setting { type T = List[String] }
  type StringSetting      <: Setting { type T = String }
  type PrefixSetting      <: Setting { type T = List[String] }
  type VersionSetting     <: Setting { type T = Version }

  type OutputDirs
  type OutputSetting <: Setting

  def BooleanSetting(name: String, descr: String): BooleanSetting
  def ChoiceSetting(name: String, helpArg: String, descr: String, choices: List[String], default: String): ChoiceSetting
  def IntSetting(name: String, descr: String, default: Int, range: Option[(Int, Int)], parser: String => Option[Int]): IntSetting
  def MultiStringSetting(name: String, helpArg: String, descr: String): MultiStringSetting
  def OutputSetting(outputDirs: OutputDirs, default: String): OutputSetting
  def PathSetting(name: String, descr: String, default: String): PathSetting
  def PhasesSetting(name: String, descr: String, default: String): PhasesSetting
  def StringSetting(name: String, helpArg: String, descr: String, default: String): StringSetting
  def PrefixSetting(name: String, prefix: String, descr: String): PrefixSetting
  def VersionSetting(name: String, helpArg: String, descr: String, default: Version): VersionSetting

  /** **/
  abstract class SettingGroup(val prefix: String) extends AbsSetting {
    def name = prefix
    def helpDescription: String = sys.error("todo")
    def unparse: List[String] = List(name)
  }
}
