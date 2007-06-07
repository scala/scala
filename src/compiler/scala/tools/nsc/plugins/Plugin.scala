package scala.tools.nsc.plugins
import java.io.File
import java.util.jar.JarFile
import java.util.zip.ZipException
import scala.xml.XML
import java.net.URLClassLoader
import scala.collection.mutable
import mutable.ListBuffer

/** Information about a plugin loaded from a jar file.
 *
 *  The concrete subclass must have a one-argument constructor
 *  that accepts an instance of Global.
 *
 *    (val global: Global)
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
  def processOptions(options: List[String], error: String=>Unit) {
    if (!options.isEmpty)
      error("Error: " + name + " has no options")
  }

  /** A description of this plugin's options, suitable as a response
   *  to the -help command-line option.  Conventionally, the
   *  options should be listed with the -P:plugname: part included.
   */
  val optionsHelp: Option[String] = None
}

object Plugin {
  /** Create a class loader with the specified file plus
   *  the loader that loaded the Scala compiler.
   */
  private def loaderFor(jarfiles: Seq[File]): ClassLoader = {
    val compilerLoader = classOf[Plugin].getClassLoader
    val jarurls = jarfiles.map(.toURL).toArray
    new URLClassLoader(jarurls, compilerLoader)
  }



  /** Try to load a plugin description from the specified
   *  file, returning None if it does not work. */
  private def loadDescription(jarfile: File): Option[PluginDescription] = {
    if (!jarfile.exists) return None

    try {
      val jar = new JarFile(jarfile)
      try {
	val ent = jar.getEntry("scalac-plugin.xml")
	if(ent == null) return None

	val inBytes = jar.getInputStream(ent)
	val packXML = XML.load(inBytes)
	inBytes.close()

	PluginDescription.fromXML(packXML)
      } finally {
	jar.close()
      }
    } catch {
      case _:ZipException => None
    }
  }



  /** Loads a plugin class from the named jar file.  Returns None
   *  if the jar file has no plugin in it or if the plugin
   *  is badly formed. */
  def loadFrom(jarfile: File,
	       loader: ClassLoader): Option[Class]  =
  {
    val pluginInfo = loadDescription(jarfile).get

    try {
      Some(loader.loadClass(pluginInfo.classname))
    } catch {
      case _:ClassNotFoundException =>
	println("Warning: class not found for plugin in " + jarfile +
                " (" + pluginInfo.classname + ")")
      None
    }
  }



  /** Load all plugins found in the argument list, bot hin
   *  the jar files explicitly listed, and in the jar files in
   *  the directories specified.  Skips all plugins in `ignoring'.
   *  A single classloader is created and used to load all of them.  */
  def loadAllFrom(jars: List[File],
		  dirs: List[File],
		  ignoring: List[String]): List[Class] =
  {
    val alljars = new ListBuffer[File]

    alljars ++= jars

    for {
      dir <- dirs
      entries = dir.listFiles.toList
      sorted = entries.sort((f1,f2)=>f1.getName <= f2.getName)
      ent <- sorted
      if ent.toString.toLowerCase.endsWith(".jar")
      pdesc <- loadDescription(ent)
      if !(ignoring contains pdesc.name)
    } alljars += ent

    val loader = loaderFor(alljars.toList)
    alljars.toList.map(f => loadFrom(f,loader)).flatMap(x => x)
  }



  /** Instantiate a plugin class, given the class and
   *  the compiler it is to be used in.
   */
  def instantiate(clazz: Class, global: Global): Plugin = {
    val constructor = clazz.getConstructor(Array(classOf[Global]))
    constructor.newInstance(Array(global)).asInstanceOf[Plugin]
  }
}
