/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author Lex Spoon
 * Updated by Anders Bach Nielsen
 */

package scala.tools.nsc
package plugins

import scala.reflect.io.Path
import scala.tools.nsc.util.ClassPath
import scala.tools.util.PathResolver.Defaults

/** Support for run-time loading of compiler plugins.
 *
 *  @author Lex Spoon
 *  @version 1.1, 2009/1/2
 *  Updated 2009/1/2 by Anders Bach Nielsen: Added features to implement SIP 00002
 */
trait Plugins { global: Global =>

  /** Load a rough list of the plugins.  For speed, it
   *  does not instantiate a compiler run.  Therefore it cannot
   *  test for same-named phases or other problems that are
   *  filtered from the final list of plugins.
   */
  protected def loadRoughPluginsList(): List[Plugin] = {
    def asPath(p: String) = ClassPath split p
    val paths  = settings.plugin.value filter (_ != "") map (s => asPath(s) map Path.apply)
    val dirs   = {
      def injectDefault(s: String) = if (s.isEmpty) Defaults.scalaPluginPath else s
      asPath(settings.pluginsDir.value) map injectDefault map Path.apply
    }
    val maybes = Plugin.loadAllFrom(paths, dirs, settings.disable.value)
    val (goods, errors) = maybes partition (_.isSuccess)
    // Explicit parameterization of recover to avoid -Xlint warning about inferred Any
    errors foreach (_.recover[Any] {
      // legacy behavior ignores altogether, so at least warn devs
      case e: MissingPluginException => if (global.isDeveloper) warning(e.getMessage)
      case e: Exception              => inform(e.getMessage)
    })
    val classes = goods map (_.get)  // flatten

    // Each plugin must only be instantiated once. A common pattern
    // is to register annotation checkers during object construction, so
    // creating multiple plugin instances will leave behind stale checkers.
    classes map (Plugin.instantiate(_, this))
  }

  protected lazy val roughPluginsList: List[Plugin] = loadRoughPluginsList()

  /** Load all available plugins.  Skips plugins that
   *  either have the same name as another one, or which
   *  define a phase name that another one does.
   */
  protected def loadPlugins(): List[Plugin] = {
    // remove any with conflicting names or subcomponent names
    def pick(
      plugins: List[Plugin],
      plugNames: Set[String],
      phaseNames: Set[String]): List[Plugin] =
    {
      if (plugins.isEmpty) return Nil // early return

      val plug :: tail      = plugins
      val plugPhaseNames    = Set(plug.components map (_.phaseName): _*)
      def withoutPlug       = pick(tail, plugNames, plugPhaseNames)
      def withPlug          = plug :: pick(tail, plugNames + plug.name, phaseNames ++ plugPhaseNames)
      lazy val commonPhases = phaseNames intersect plugPhaseNames

      def note(msg: String): Unit = if (settings.verbose) inform(msg format plug.name)
      def fail(msg: String)       = { note(msg) ; withoutPlug }

      if (plugNames contains plug.name)
        fail("[skipping a repeated plugin: %s]")
      else if (settings.disable.value contains plug.name)
        fail("[disabling plugin: %s]")
      else if (!commonPhases.isEmpty)
        fail("[skipping plugin %s because it repeats phase names: " + (commonPhases mkString ", ") + "]")
      else {
        note("[loaded plugin %s]")
        withPlug
      }
    }

    val plugs = pick(roughPluginsList, Set(), (phasesSet map (_.phaseName)).toSet)

    // Verify required plugins are present.
    for (req <- settings.require.value ; if !(plugs exists (_.name == req)))
      globalError("Missing required plugin: " + req)

    // Verify no non-existent plugin given with -P
    for {
      opt <- settings.pluginOptions.value
      if !(plugs exists (opt startsWith _.name + ":"))
    } globalError("bad option: -P:" + opt)

    // Plugins may opt out, unless we just want to show info
    plugs filter (p => p.init(p.options, globalError) || (settings.debug && settings.isInfo))
  }

  lazy val plugins: List[Plugin] = loadPlugins()

  /** A description of all the plugins that are loaded */
  def pluginDescriptions: String =
    roughPluginsList map (x => "%s - %s".format(x.name, x.description)) mkString "\n"

  /**
   * Extract all phases supplied by plugins and add them to the phasesSet.
   * @see phasesSet
   */
  protected def computePluginPhases(): Unit =
    for (p <- plugins; c <- p.components) addToPhasesSet(c, c.description)

  /** Summary of the options for all loaded plugins */
  def pluginOptionsHelp: String =
    (for (plug <- roughPluginsList ; help <- plug.optionsHelp) yield {
      "\nOptions for plugin '%s':\n%s\n".format(plug.name, help)
    }).mkString
}
