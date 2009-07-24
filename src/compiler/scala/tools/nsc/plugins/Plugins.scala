/* NSC -- new Scala compiler
 * Copyright 2007-2009 LAMP/EPFL
 * @author Lex Spoon
 * Updated by Anders Bach Nielsen
 */
// $Id$

package scala.tools.nsc
package plugins

import java.io.File

/** Support for run-time loading of compiler plugins.
 *
 *  @author Lex Spoon
 *  @version 1.1, 2009/1/2
 *  Updated 2009/1/2 by Anders Bach Nielsen: Added features to implement SIP 00002
 */
trait Plugins { self: Global =>

  /** Load a rough list of the plugins.  For speed, it
   *  does not instantiate a compiler run.  Therefore it cannot
   *  test for same-named phases or other problems that are
   *  filtered from the final list of plugins.
   */
  protected def loadRoughPluginsList(): List[Plugin] = {
    val jars = settings.plugin.value.map(new File(_))
    val dirs =
      for (name <- settings.pluginsDir.value.split(File.pathSeparator).toList)
	yield new File(name)

    for (plugClass <- Plugin.loadAllFrom(jars, dirs, settings.disable.value))
    yield Plugin.instantiate(plugClass, this)
  }

  private var roughPluginsListCache: Option[List[Plugin]] = None

  protected def roughPluginsList: List[Plugin] =
    roughPluginsListCache match {
      case Some(list) => list
      case None =>
	roughPluginsListCache = Some(loadRoughPluginsList)
        roughPluginsListCache.get
    }

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
      plugins match {
	case Nil => Nil
	case plug :: rest =>
	  val plugPhaseNames = Set.empty ++ plug.components.map(_.phaseName)
	  def withoutPlug = pick(rest, plugNames, plugPhaseNames)
	  def withPlug =
	    (plug ::
	     pick(rest,
		  plugNames+plug.name,
		  phaseNames++plugPhaseNames))

	  if (plugNames.contains(plug.name)) {
	    if (settings.verbose.value)
	      inform("[skipping a repeated plugin: " + plug.name + "]")
	    withoutPlug
	  } else if (settings.disable.value contains(plug.name)) {
	    if (settings.verbose.value)
	      inform("[disabling plugin: " + plug.name + "]")
	    withoutPlug
	  } else {
	    val commonPhases = phaseNames.intersect(plugPhaseNames)
	    if (!commonPhases.isEmpty) {
	      if (settings.verbose.value)
		inform("[skipping plugin " + plug.name +
		       "because it repeats phase names: " +
		       commonPhases.mkString(", ") + "]")
	      withoutPlug
	    } else {
	      if (settings.verbose.value)
		inform("[loaded plugin " + plug.name + "]")
	      withPlug
	    }
	  }
      }
    }

    val plugs =
    pick(roughPluginsList,
	 Set.empty,
	 Set.empty ++ phasesSet.map(_.phaseName))

    for (req <- settings.require.value; if !plugs.exists(p => p.name==req))
      error("Missing required plugin: " + req)


    for (plug <- plugs) {
      val nameColon = plug.name + ":"
      val opts = for {
	raw <- settings.pluginOptions.value
	if raw.startsWith(nameColon)
      } yield raw.substring(nameColon.length)

      if (!opts.isEmpty)
	plug.processOptions(opts, error)
    }

    for {
      opt <- settings.pluginOptions.value
      if !plugs.exists(p => opt.startsWith(p.name + ":"))
    } error("bad option: -P:" + opt)

    plugs
  }

  private var pluginsCache: Option[List[Plugin]] = None

  def plugins: List[Plugin] = {
    if (pluginsCache.isEmpty)
      pluginsCache = Some(loadPlugins)
    pluginsCache.get
  }

  /** A description of all the plugins that are loaded */
  def pluginDescriptions: String = {
    val messages =
      for (plugin <- roughPluginsList)
	yield plugin.name + " - " + plugin.description
    messages.mkString("\n")
  }

  /**
   * Extract all phases supplied by plugins and add them to the phasesSet.
   * @see phasesSet
   */
  protected def computePluginPhases() {
    val plugPhases = plugins.flatMap(_.components)
    for (pPhase <- plugPhases) {
      phasesSet += pPhase
     }
   }

  /** Summary of the options for all loaded plugins */
  def pluginOptionsHelp: String = {
    val buf = new StringBuffer
    for (plug <- roughPluginsList; help <- plug.optionsHelp) {
      buf append ("Options for plugin " + plug.name + ":\n")
      buf append help
      buf append "\n"
    }
    buf.toString
  }
}
