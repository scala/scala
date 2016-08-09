/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author Lex Spoon
 */

package scala.tools.nsc
package plugins

import scala.tools.nsc.io.Jar
import scala.reflect.internal.util.ScalaClassLoader
import scala.reflect.io.{ Directory, File, Path }
import java.io.InputStream

import scala.collection.mutable
import scala.util.{ Try, Success, Failure }

/** Information about a plugin loaded from a jar file.
 *
 *  The concrete subclass must have a one-argument constructor
 *  that accepts an instance of `global`.
 *  {{{
 *    (val global: Global)
 *  }}}
 *
 *  @author Lex Spoon
 *  @version 1.0, 2007-5-21
 */
abstract class Plugin {
  /** The name of this plugin */
  val name: String

  /** The components that this phase defines */
  val components: List[PluginComponent]

  /** A one-line description of the plugin */
  val description: String

  /** The compiler that this plugin uses.  This is normally equated
   *  to a constructor parameter in the concrete subclass.
   */
  val global: Global

  def options: List[String] = {
    // Process plugin options of form plugin:option
    def namec = name + ":"
    global.settings.pluginOptions.value filter (_ startsWith namec) map (_ stripPrefix namec)
  }

  /** Handle any plugin-specific options.
   *  The user writes `-P:plugname:opt1,opt2`,
   *  but the plugin sees `List(opt1, opt2)`.
   *  The plugin can opt out of further processing
   *  by returning false.  For example, if the plugin
   *  has an "enable" flag, now would be a good time
   *  to sit on the bench.
   *  @param options plugin arguments
   *  @param error error function
   *  @return true to continue, or false to opt out
   */
  def init(options: List[String], error: String => Unit): Boolean = {
    // call to deprecated method required here, we must continue to support
    // code that subclasses that override `processOptions`.
    processOptions(options, error)
    true
  }

  @deprecated("use Plugin#init instead", since="2.11.0")
  def processOptions(options: List[String], error: String => Unit): Unit = {
    if (!options.isEmpty) error(s"Error: $name takes no options")
  }

  /** A description of this plugin's options, suitable as a response
   *  to the -help command-line option.  Conventionally, the options
   *  should be listed with the `-P:plugname:` part included.
   */
  val optionsHelp: Option[String] = None
}

/** ...
 *
 *  @author Lex Spoon
 *  @version 1.0, 2007-5-21
 */
object Plugin {

  private val PluginXML = "scalac-plugin.xml"

  /** Create a class loader with the specified locations plus
   *  the loader that loaded the Scala compiler.
   */
  private def loaderFor(locations: Seq[Path]): ScalaClassLoader = {
    val compilerLoader = classOf[Plugin].getClassLoader
    val urls = locations map (_.toURL)

    ScalaClassLoader fromURLs (urls, compilerLoader)
  }

  /** Try to load a plugin description from the specified location.
   */
  private def loadDescriptionFromJar(jarp: Path): Try[PluginDescription] = {
    // XXX Return to this once we have more ARM support
    def read(is: Option[InputStream]) = is match {
      case None     => throw new PluginLoadException(jarp.path, s"Missing $PluginXML in $jarp")
      case Some(is) => PluginDescription.fromXML(is)
    }
    Try(new Jar(jarp.jfile).withEntryStream(PluginXML)(read))
  }

  private def loadDescriptionFromFile(f: Path): Try[PluginDescription] =
    Try(PluginDescription.fromXML(new java.io.FileInputStream(f.jfile)))

  type AnyClass = Class[_]

  /** Use a class loader to load the plugin class.
   */
  def load(classname: String, loader: ClassLoader): Try[AnyClass] = {
    import scala.util.control.NonFatal
    try {
      Success[AnyClass](loader loadClass classname)
    } catch {
      case NonFatal(e) =>
        Failure(new PluginLoadException(classname, s"Error: unable to load class: $classname"))
      case e: NoClassDefFoundError =>
        Failure(new PluginLoadException(classname, s"Error: class not found: ${e.getMessage} required by $classname"))
    }
  }

  /** Load all plugins specified by the arguments.
   *  Each location of `paths` must be a valid plugin archive or exploded archive.
   *  Each of `paths` must define one plugin.
   *  Each of `dirs` may be a directory containing arbitrary plugin archives.
   *  Skips all plugins named in `ignoring`.
   *  A classloader is created to load each plugin.
   */
  def loadAllFrom(
    paths: List[List[Path]],
    dirs: List[Path],
    ignoring: List[String]): List[Try[AnyClass]] =
  {
    // List[(jar, Try(descriptor))] in dir
    def scan(d: Directory) =
      d.files.toList sortBy (_.name) filter (Jar isJarOrZip _) map (j => (j, loadDescriptionFromJar(j)))

    type PDResults = List[Try[(PluginDescription, ScalaClassLoader)]]

    // scan plugin dirs for jars containing plugins, ignoring dirs with none and other jars
    val fromDirs: PDResults = dirs filter (_.isDirectory) flatMap { d =>
      scan(d.toDirectory) collect {
        case (j, Success(pd)) => Success((pd, loaderFor(Seq(j))))
      }
    }

    // scan jar paths for plugins, taking the first plugin you find.
    // a path element can be either a plugin.jar or an exploded dir.
    def findDescriptor(ps: List[Path]) = {
      def loop(qs: List[Path]): Try[PluginDescription] = qs match {
        case Nil       => Failure(new MissingPluginException(ps))
        case p :: rest =>
          if (p.isDirectory) loadDescriptionFromFile(p.toDirectory / PluginXML) orElse loop(rest)
          else if (p.isFile) loadDescriptionFromJar(p.toFile) orElse loop(rest)
          else loop(rest)
      }
      loop(ps)
    }
    val fromPaths: PDResults = paths map (p => (p, findDescriptor(p))) map {
      case (p, Success(pd)) => Success((pd, loaderFor(p)))
      case (_, Failure(e))  => Failure(e)
    }

    val seen = mutable.HashSet[String]()
    val enabled = (fromPaths ::: fromDirs) map {
      case Success((pd, loader)) if seen(pd.classname)        =>
        // a nod to SI-7494, take the plugin classes distinctly
        Failure(new PluginLoadException(pd.name, s"Ignoring duplicate plugin ${pd.name} (${pd.classname})"))
      case Success((pd, loader)) if ignoring contains pd.name =>
        Failure(new PluginLoadException(pd.name, s"Disabling plugin ${pd.name}"))
      case Success((pd, loader)) =>
        seen += pd.classname
        Plugin.load(pd.classname, loader)
      case Failure(e)            =>
        Failure(e)
    }
    enabled   // distinct and not disabled
  }

  /** Instantiate a plugin class, given the class and
   *  the compiler it is to be used in.
   */
  def instantiate(clazz: AnyClass, global: Global): Plugin = {
    (clazz getConstructor classOf[Global] newInstance global).asInstanceOf[Plugin]
  }
}

class PluginLoadException(val path: String, message: String, cause: Exception) extends Exception(message, cause) {
  def this(path: String, message: String) = this(path, message, null)
}

class MissingPluginException(path: String) extends PluginLoadException(path, s"No plugin in path $path") {
  def this(paths: List[Path]) = this(paths mkString File.pathSeparator)
}
