/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package settings

trait AdvancedScalaSettings {
  self: AbsScalaSettings =>

  abstract class X extends SettingGroup("-X") {
    val assemextdirs: StringSetting
    val assemname: StringSetting
    val assempath: StringSetting
    val checkinit: BooleanSetting
    val disableassertions: BooleanSetting
    val elidebelow: IntSetting
    val experimental: BooleanSetting
    val future: BooleanSetting
    val generatephasegraph: StringSetting
    val logimplicits: BooleanSetting
    val mainClass: StringSetting
    val migration: VersionSetting
    val noforwarders: BooleanSetting
    val nojline: BooleanSetting
    val nouescape: BooleanSetting
    val plugin: MultiStringSetting
    val plugindisable: MultiStringSetting
    val pluginlist: BooleanSetting
    val pluginrequire: MultiStringSetting
    val pluginsdir: StringSetting
    val print: PhasesSetting
    val printicode: BooleanSetting
    val printpos: BooleanSetting
    val printtypes: BooleanSetting
    val prompt: BooleanSetting
    val resident: BooleanSetting
    val script: StringSetting
    val showclass: StringSetting
    val showobject: StringSetting
    val showphases: BooleanSetting
    val sourcedir: StringSetting
    val sourcereader: StringSetting
  }
  // def Xexperimental = X.experimental
  // def Xnojline = X.nojline
  // def Xprint = X.print
  // def Xprintpos = X.printpos
  // def Xshowcls = X.showclass
  // def Xshowobj = X.showobject
  // def assemextdirs = X.assemextdirs
  // def assemname = X.assemname
  // def assemrefs = X.assempath
  // def checkInit = X.checkinit
  // def disable = X.plugindisable
  // def elideLevel = X.elidelevel
  // def future = X.future
  // def genPhaseGraph = X.generatephasegraph
  // def logimplicits = X.logimplicits
  // def noForwarders = X.noforwarders
  // def noassertions = X.disableassertions
  // def nouescape = X.nouescape
  // def plugin = X.plugin
  // def pluginsDir = X.pluginsdir
  // def printtypes = X.printtypes
  // def prompt = X.prompt
  // def require = X.require
  // def resident = X.resident
  // def script = X.script
  // def showPhases = X.showphases
  // def showPlugins = X.pluginlist
  // def sourceReader = X.sourcereader
  // def sourcedir = X.sourcedir
  // def writeICode = X.printicode
}