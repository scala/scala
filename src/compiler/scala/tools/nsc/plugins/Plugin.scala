/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
 * @author Lex Spoon
 */

package scala.tools.nsc
package plugins

import scala.tools.nsc.io.{ Jar }
import scala.tools.nsc.util.ScalaClassLoader
import scala.reflect.io.{ Directory, File, Path }
import java.io.InputStream
import java.util.zip.ZipException

import scala.collection.mutable.ListBuffer
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
    processOptions(options, error)
    true
  }

  @deprecated("use Plugin#init instead", since="2.11")
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
      case None     => throw new RuntimeException(s"Missing $PluginXML in $jarp")
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
    Try[AnyClass] {
      loader loadClass classname
    } recoverWith {
      case _: Exception =>
        Failure(new RuntimeException(s"Warning: class not found: ${classname}"))
    }
  }

  /** Load all plugins specified by the arguments.
   *  Each of `jars` must be a valid plugin archive or exploded archive.
   *  Each of `dirs` may be a directory containing arbitrary plugin archives.
   *  Skips all plugins named in `ignoring`.
   *  A single classloader is created and used to load all of them.
   */
  def loadAllFrom(
    jars: List[Path],
    dirs: List[Path],
    ignoring: List[String]): List[Try[AnyClass]] =
  {
    // List[(jar, Success(descriptor))] in dir
    def scan(d: Directory) = for {
      f  <- d.files.toList sortBy (_.name)
      if Jar isJarOrZip f
      pd = loadDescriptionFromJar(f)
      if pd.isSuccess
    } yield (f, pd)
    // (dir, Try(descriptor))
    def explode(d: Directory) = d -> loadDescriptionFromFile(d / PluginXML)
    // (j, Try(descriptor))
    def required(j: Path) = j -> loadDescriptionFromJar(j)

    type Paired = Pair[Path, Try[PluginDescription]]
    val included: List[Paired] = (dirs flatMap (_ ifDirectory scan)).flatten
    val exploded: List[Paired] = jars flatMap (_ ifDirectory explode)
    val explicit: List[Paired] = jars flatMap (_ ifFile required)
    def ignored(p: Paired) = p match {
      case (path, Success(pd)) => ignoring contains pd.name
      case _                   => false
    }
    val (locs, pds) = ((explicit ::: exploded ::: included) filterNot ignored).unzip
    val loader = loaderFor(locs.distinct)
    (pds filter (_.isSuccess) map (_.get.classname)).distinct map (Plugin load (_, loader))
  }

  /** Instantiate a plugin class, given the class and
   *  the compiler it is to be used in.
   */
  def instantiate(clazz: AnyClass, global: Global): Plugin = {
    (clazz getConstructor classOf[Global] newInstance global).asInstanceOf[Plugin]
  }
}
