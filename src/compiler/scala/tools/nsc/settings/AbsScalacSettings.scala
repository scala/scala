/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package settings

trait AbsScalacSettings {
  self: AbsSettings =>

  type BooleanSetting <: AbsSetting
  type ChoiceSetting <: AbsSetting
  type DebugSetting <: AbsSetting
  type DefinesSetting <: AbsSetting
  type IntSetting <: AbsSetting
  type MultiStringSetting <: AbsSetting
  type PathSetting <: AbsSetting
  type PhasesSetting <: AbsSetting
  type StringSetting <: AbsSetting

  type OutputDirs
  type OutputSetting <: AbsSetting

  def BooleanSetting(name: String, descr: String): BooleanSetting
  def ChoiceSetting(name: String, descr: String, choices: List[String], default: String): ChoiceSetting
  def DebugSetting(name: String, descr: String, choices: List[String], default: String, defaultEmpty: String): DebugSetting
  def DefinesSetting(): DefinesSetting
  def IntSetting(name: String, descr: String, default: Int, range: Option[(Int, Int)], parser: String => Option[Int]): IntSetting
  def MultiStringSetting(name: String, arg: String, descr: String): MultiStringSetting
  def OutputSetting(outputDirs: OutputDirs, default: String): OutputSetting
  def PathSetting(name: String, arg: String, descr: String, default: String): PathSetting
  def PhasesSetting(name: String, descr: String): PhasesSetting
  def StringSetting(name: String, arg: String, descr: String, default: String): StringSetting
}
