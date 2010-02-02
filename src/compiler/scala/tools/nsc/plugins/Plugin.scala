/* NSC -- new Scala compiler
 * Copyright 2007-2010 LAMP/EPFL
 * @author Lex Spoon
 */
// $Id$

package scala.tools.nsc
package plugins

import io.{ File, Path }
import java.net.URLClassLoader
import java.util.jar.JarFile
import java.util.zip.ZipException

import scala.collection.mutable
import mutable.ListBuffer
import scala.xml.XML

/** <p>
 *    Information about a plugin loaded from a jar file.
 *  </p>
 *  <p>
 *    The concrete subclass must have a one-argument constructor
 *    that accepts an instance of <code>Global</code>.
 *  </p><pre>
 *    (val global: Global)
 *  </pre>
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
   *  to a constructor parameter in the concrete subclass. */
  val global: Global

  /** Handle any plugin-specific options.  The -P:plugname: part
   *  will not be present. */
  def processOptions(options: List[String], error: String => Unit) {
    if (!options.isEmpty)
      error("Error: " + name + " has no options")
  }

  /** A description of this plugin's options, suitable as a response
   *  to the -help command-line option.  Conventionally, the
   *  options should be listed with the <code>-P:plugname:</code>
   *  part included.
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

  /** Create a class loader with the specified file plus
   *  the loader that loaded the Scala compiler.
   */
  private def loaderFor(jarfiles: Seq[Path]): ClassLoader = {
    val compilerLoader = classOf[Plugin].getClassLoader
    val jarurls = jarfiles map (_.toURL)

    new URLClassLoader(jarurls.toArray, compilerLoader)
  }

  /** Try to load a plugin description from the specified
   *  file, returning <code>None</code> if it does not work.
   */
  private def loadDescription(jarfile: Path): Option[PluginDescription] =
    // XXX Return to this once we have some ARM support
    if (!jarfile.exists) None
    else try {
      val jar = new JarFile(jarfile.jfile)

      try {
        jar getEntry PluginXML match {
          case null  => None
          case entry =>
            val in = jar getInputStream entry
            val packXML = XML load in
            in.close()

            PluginDescription fromXML packXML
        }
      }
      finally jar.close()
    }
    catch {
      case _: ZipException => None
    }

  type AnyClass = Class[_]

  /** Loads a plugin class from the named jar file.

   *  @return <code>None</code> if the jar file has no plugin in it or
   *                            if the plugin is badly formed.
   */
  def loadFrom(jarfile: Path, loader: ClassLoader): Option[AnyClass] =
    loadDescription(jarfile) match {
      case None => None
      case Some(pdesc) =>
        try Some(loader loadClass pdesc.classname) catch {
        case _: Exception =>
          println("Warning: class not found for plugin in %s (%s)".format(jarfile, pdesc.classname))
          None
      }
    }

  /** Load all plugins found in the argument list, both in the
   *  jar files explicitly listed, and in the jar files in the
   *  directories specified. Skips all plugins in <code>ignoring</code>.
   *  A single classloader is created and used to load all of them.
   */
  def loadAllFrom(
    jars: List[Path],
    dirs: List[Path],
    ignoring: List[String]): List[AnyClass] =
  {
    val alljars = jars ::: (for {
      dir <- dirs if dir.isDirectory
      entry <- dir.toDirectory.files.toList sortBy (_.name)
      if entry.extension == "jar"
      pdesc <- loadDescription(entry)
      if !(ignoring contains pdesc.name)
    } yield entry)

    val loader = loaderFor(alljars)
    alljars map (loadFrom(_, loader)) flatten
  }

  /** Instantiate a plugin class, given the class and
   *  the compiler it is to be used in.
   */
  def instantiate(clazz: AnyClass, global: Global): Plugin = {
    val constructor = clazz getConstructor classOf[Global]
    (constructor newInstance global).asInstanceOf[Plugin]
  }
}
