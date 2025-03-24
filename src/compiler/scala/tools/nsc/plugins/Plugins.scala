/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package plugins

import java.net.URL
import java.util

import scala.reflect.internal.util.ScalaClassLoader
import scala.reflect.io.Path
import scala.tools.nsc.Reporting.WarningCategory
import scala.tools.nsc.plugins.Plugin.pluginClassLoadersCache
import scala.tools.nsc.typechecker.Macros
import scala.tools.nsc.util.ClassPath
import scala.tools.util.PathResolver.Defaults

/** Support for run-time loading of compiler plugins.
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
    val maybes = Plugin.loadAllFrom(paths, dirs, settings.disable.value, findPluginClassLoader(_))
    val (goods, errors) = maybes partition (_.isSuccess)
    // Explicit parameterization of recover to avoid -Xlint warning about inferred Any
    errors foreach (_.recover[Any] {
      // legacy behavior ignores altogether, so at least warn devs
      case e: MissingPluginException => if (global.isDeveloper) runReporting.warning(NoPosition, e.getMessage, WarningCategory.OtherDebug, site = "")
      case e: Exception              => inform(e.getMessage)
    })
    val classes = goods map (_.get)  // flatten

    // Each plugin must only be instantiated once. A common pattern
    // is to register annotation checkers during object construction, so
    // creating multiple plugin instances will leave behind stale checkers.
    classes map (Plugin.instantiate(_, this))
  }

  /**
    * Locate or create the classloader to load a compiler plugin with `classpath`.
    *
    * Subclasses may override to customise the behaviour. The returned classloader must return the first
    * from plugin descriptor from `classpath` when `getResource("scalac-plugin.xml")` is called.
    *
    * @param classpath
    * @return
    */
  protected def findPluginClassLoader(classpath: Seq[Path]): ClassLoader = {
    val policy = settings.YcachePluginClassLoader.value
    val disableCache = policy == settings.CachePolicy.None.name
    def newLoader = () => {
      val compilerLoader = classOf[Plugin].getClassLoader
      val urls = classpath map (_.toURL)
      new ScalaClassLoader.URLClassLoader(urls, compilerLoader) {
        // scala/bug#11666 no parent delegation for plugin.xml to avoid getting plugin.xml from parent classloader

        override def getResources(name: String): util.Enumeration[URL] = {
          if (name == Plugin.PluginXML) findResources(name);
          else super.getResources(name)
        }

        override def getResource(name: String): URL = {
          if (name == Plugin.PluginXML) findResource(name);
          else super.getResource(name)
        }
      }
    }

    // Create a class loader with the specified locations plus
    // the loader that loaded the Scala compiler.
    //
    // If the class loader has already been created before and the
    // file stamps are the same, the previous loader is returned to
    // mitigate the cost of dynamic classloading as it has been
    // measured in https://github.com/scala/scala-dev/issues/458.

    val cache = pluginClassLoadersCache
    val checkStamps = policy == settings.CachePolicy.LastModified.name
    cache.checkCacheability(classpath.map(_.toURL), checkStamps, disableCache) match {
      case Left(_) =>
        val loader = newLoader()
        closeableRegistry.registerCloseable(loader)
        loader
      case Right(paths) =>
        cache.getOrCreate((), paths, newLoader, closeableRegistry, checkStamps)
    }
  }

  protected lazy val roughPluginsList: List[Plugin] = loadRoughPluginsList()

  /** Load all available plugins.  Skips plugins that
   *  either have the same name as another one, or which
   *  define a phase name that another one does.
   */
  protected def loadPlugins(): List[Plugin] = {
    // remove any with conflicting names or subcomponent names
    def pick(plugins: List[Plugin], plugNames: Set[String], phaseNames: Set[String]): List[Plugin] = plugins match {
      case Nil          => Nil // early return
      case plug :: tail =>
        val plugPhaseNames    = Set(plug.components map (_.phaseName): _*)
        def withoutPlug       = pick(tail, plugNames, plugPhaseNames)
        def withPlug          = plug :: pick(tail, plugNames + plug.name, phaseNames ++ plugPhaseNames)
        lazy val commonPhases = phaseNames intersect plugPhaseNames

        def note(msg: String): Unit = if (settings.verbose.value) inform(msg format plug.name)
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
    plugs filter (p => p.init(p.options, globalError) || (settings.isDebug && settings.isInfo))
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

  /** Obtains a `ClassLoader` instance used for macro expansion.
    *
    *  By default a new `ScalaClassLoader` is created using the classpath
    *  from global and the classloader of self as parent.
    *
    *  Mirrors with runtime definitions (e.g. Repl) need to adjust this method.
    */
  def findMacroClassLoader(): ClassLoader = {
    val classpath: Seq[URL] = if (settings.YmacroClasspath.isSetByUser) {
      for {
        file <- ClassPath.expandPath(settings.YmacroClasspath.value, expandStar = true)
        af <- Option(settings.pathFactory.getDirectory(file))
      } yield af.file.toURI.toURL
    } else global.classPath.asURLs
    def newLoader: () => ScalaClassLoader.URLClassLoader = () => {
      analyzer.macroLogVerbose("macro classloader: initializing from -cp: %s".format(classpath))
      ScalaClassLoader.fromURLs(classpath, getClass.getClassLoader)
    }

    val policy = settings.YcacheMacroClassLoader.value
    val cache = Macros.macroClassLoadersCache
    val disableCache = policy == settings.CachePolicy.None.name
    val checkStamps = policy == settings.CachePolicy.LastModified.name
    cache.checkCacheability(classpath, checkStamps, disableCache) match {
      case Left(msg) =>
        analyzer.macroLogVerbose(s"macro classloader: $msg.")
        val loader = newLoader()
        closeableRegistry.registerCloseable(loader)
        loader
      case Right(paths) =>
        cache.getOrCreate((), paths, newLoader, closeableRegistry, checkStamps)
    }
  }
}
