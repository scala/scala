/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author Stephane Micheloud
 */

package scala.tools.detach

import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}

class DetachPlugin(val global: Global) extends Plugin {
  import global._

  val name = "detach"
  val description = "Perform detaching of remote closures"

  object detach extends {
    val global = DetachPlugin.this.global
    val runsAfter = List("lambdalift")
    override val runsBefore = List("constructors")
  } with Detach

  val components = List[PluginComponent](detach)

  def setEnabled(flag: Boolean) { detach.isEnabled = flag }

  override def processOptions(options: List[String], error: String => Unit) = {
    var enabled = false
    for (option <- options) {
      if (option == "enable") {
        enabled = true
      } else {
        error("Option not understood: "+option)
      }
    }
    setEnabled(enabled)
  }

  override val optionsHelp: Option[String] =
    Some("  -P:detach:enable               Enable detaching of remote closures")
}
