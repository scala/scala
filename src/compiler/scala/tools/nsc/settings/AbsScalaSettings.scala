/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala
package tools.nsc
package settings

import scala.language.higherKinds

trait AbsScalaSettings {
  self: AbsSettings =>

  type MultiChoiceEnumeration <: Enumeration

  type Setting <: AbsSetting

  type BooleanSetting                                  <: Setting { type T = Boolean }
  type ChoiceSetting                                   <: Setting { type T = String }
  type IntSetting                                      <: Setting { type T = Int }
  type MultiStringSetting                              <: Setting { type T = List[String] }
  type MultiChoiceSetting[E <: MultiChoiceEnumeration] <: Setting { type T <: E#ValueSet }
  type PathSetting                                     <: Setting { type T = String }
  type PhasesSetting                                   <: Setting { type T = List[String] }
  type StringSetting                                   <: Setting { type T = String }
  type PrefixSetting                                   <: Setting { type T = List[String] }

  type OutputDirs
  type OutputSetting <: Setting

  def BooleanSetting(name: String, descr: String): BooleanSetting
  def ChoiceSetting(name: String, helpArg: String, descr: String, choices: List[String], default: String, choicesHelp: List[String] = Nil): ChoiceSetting
  def ChoiceSettingForcedDefault(name: String, helpArg: String, descr: String, choices: List[String], default: String, choicesHelp: List[String] = Nil): ChoiceSetting
  def IntSetting(name: String, descr: String, default: Int, range: Option[(Int, Int)], parser: String => Option[Int]): IntSetting
  def MultiStringSetting(name: String, helpArg: String, descr: String): MultiStringSetting
  def MultiChoiceSetting[E <: MultiChoiceEnumeration](name: String, helpArg: String, descr: String, domain: E, default: Option[List[String]]): MultiChoiceSetting[E]
  def OutputSetting(outputDirs: OutputDirs, default: String): OutputSetting
  def PathSetting(name: String, descr: String, default: String): PathSetting
  def PhasesSetting(name: String, descr: String, default: String): PhasesSetting
  def StringSetting(name: String, helpArg: String, descr: String, default: String): StringSetting
  def PrefixSetting(name: String, prefix: String, descr: String): PrefixSetting
}
