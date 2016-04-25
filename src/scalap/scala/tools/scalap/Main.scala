/*     ___ ____ ___   __   ___   ___
**    / _// __// _ | / /  / _ | / _ \  Scala classfile decoder
**  __\ \/ /__/ __ |/ /__/ __ |/ ___/  (c) 2003-2013, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_/_/      http://scala-lang.org/
**
*/

package scala
package tools.scalap

import java.io.{ByteArrayOutputStream, OutputStreamWriter, PrintStream}
import scala.reflect.NameTransformer
import scala.tools.nsc.Settings
import scala.tools.nsc.classpath.{AggregateClassPath, ClassPathFactory}
import scala.tools.nsc.util.ClassPath
import scala.tools.util.PathResolver
import scalax.rules.scalasig._

/**The main object used to execute scalap on the command-line.
 *
 * @author Matthias Zenger, Stephane Micheloud, Burak Emir, Ilya Sergey
 */
class Main {
  val SCALA_SIG            = "ScalaSig"
  val SCALA_SIG_ANNOTATION = "Lscala/reflect/ScalaSignature;"
  val SCALA_LONG_SIG_ANNOTATION = "Lscala/reflect/ScalaLongSignature;"
  val BYTES_VALUE          = "bytes"

  val versionMsg = "Scala classfile decoder %s -- %s\n".format(Properties.versionString, Properties.copyrightString)

  /**Verbose program run?
   */
  var verbose = false
  var printPrivates = false

  def isScalaFile(bytes: Array[Byte]): Boolean = {
    val byteCode  = ByteCode(bytes)
    val classFile = ClassFileParser.parse(byteCode)
    classFile.attribute("ScalaSig").isDefined
  }

  /**Processes the given Java class file.
   *
   * @param clazz the class file to be processed.
   */
  def processJavaClassFile(clazz: Classfile): Unit = {
    // construct a new output stream writer
    val out = new OutputStreamWriter(Console.out)
    val writer = new JavaWriter(clazz, out)
    // print the class
    writer.printClass()
    out.flush()
  }

  def isPackageObjectFile(s: String) = s != null && (s.endsWith(".package") || s == "package")

  def parseScalaSignature(scalaSig: ScalaSig, isPackageObject: Boolean) = {
    val baos   = new ByteArrayOutputStream
    val stream = new PrintStream(baos)
    val syms   = scalaSig.topLevelClasses ++ scalaSig.topLevelObjects

    syms.head.parent match {
      // Partial match
      case Some(p) if p.name != "<empty>" =>
        val path = p.path
        if (!isPackageObject) {
          stream.print("package ")
          stream.print(path)
          stream.print("\n")
        } else {
          val i = path.lastIndexOf(".")
          if (i > 0) {
            stream.print("package ")
            stream.print(path.substring(0, i))
            stream.print("\n")
          }
        }
      case _ =>
    }
    // Print classes
    val printer = new ScalaSigPrinter(stream, printPrivates)
    syms foreach (printer printSymbol _)
    baos.toString
  }

  def decompileScala(bytes: Array[Byte], isPackageObject: Boolean): String = {
    val byteCode = ByteCode(bytes)
    val classFile = ClassFileParser.parse(byteCode)

    ScalaSigParser.parse(classFile) match {
      case Some(scalaSig) => parseScalaSignature(scalaSig, isPackageObject)
      case None           => ""
    }
  }

  /** Executes scalap with the given arguments and classpath for the
   *  class denoted by `classname`.
   */
  def process(args: Arguments, path: ClassPath)(classname: String): Unit = {
    // find the classfile
    val encName = classname match {
      case "scala.AnyRef" => "java.lang.Object"
      case _ =>
        // we have to encode every fragment of a name separately, otherwise the NameTransformer
        // will encode using unicode escaping dot separators as well
        // we can afford allocations because this is not a performance critical code
        classname.split('.').map(NameTransformer.encode).mkString(".")
    }

    path.findClassFile(encName) match {
      case Some(classFile) =>
        if (verbose) {
          Console.println(Console.BOLD + "FILENAME" + Console.RESET + " = " + classFile.path)
        }
        val bytes = classFile.toByteArray
        if (isScalaFile(bytes)) {
          Console.println(decompileScala(bytes, isPackageObjectFile(encName)))
        } else {
          // construct a reader for the classfile content
          val reader = new ByteArrayReader(classFile.toByteArray)
          // parse the classfile
          val clazz = new Classfile(reader)
          processJavaClassFile(clazz)
        }
        // if the class corresponds to the artificial class scala.Any.
        // (see member list in class scala.tool.nsc.symtab.Definitions)
      case _ =>
        Console.println(s"class/object $classname not found.")
    }
  }
}

object Main extends Main {

  private object opts {
    val cp = "-cp"
    val help = "-help"
    val classpath = "-classpath"
    val showPrivateDefs = "-private"
    val verbose = "-verbose"
    val version = "-version"

    val disableFlatClassPathCaching = "-YdisableFlatCpCaching"
    val logClassPath = "-Ylog-classpath"
  }

  /** Prints usage information for scalap. */
  def usage(): Unit = {
    Console println s"""
      |Usage: scalap {<option>} <name>
      |where <name> is fully-qualified class name or <package_name>.package for package objects
      |and <option> is
      |  ${opts.showPrivateDefs}           print private definitions
      |  ${opts.verbose}           print out additional information
      |  ${opts.version}           print out the version number of scalap
      |  ${opts.help}              display this usage message
      |  ${opts.classpath} <path>  specify where to find user class files
      |  ${opts.cp} <path>         specify where to find user class files
    """.stripMargin.trim
  }

  def main(args: Array[String]): Unit =
  // print usage information if there is no command-line argument
    if (args.isEmpty) usage()
    else {
      val arguments = parseArguments(args)

      if (arguments contains opts.version)
        Console.println(versionMsg)
      if (arguments contains opts.help)
        usage()

      verbose = arguments contains opts.verbose
      printPrivates = arguments contains opts.showPrivateDefs
      // construct a custom class path
      val cpArg = List(opts.classpath, opts.cp) map arguments.getArgument reduceLeft (_ orElse _)

      val settings = new Settings()

      settings.YdisableFlatCpCaching.value = arguments contains opts.disableFlatClassPathCaching
      settings.Ylogcp.value = arguments contains opts.logClassPath

      val path = createClassPath(cpArg, settings)

      // print the classpath if output is verbose
      if (verbose)
        Console.println(Console.BOLD + "CLASSPATH" + Console.RESET + " = " + path.asClassPathString)

      // process all given classes
      arguments.getOthers foreach process(arguments, path)
    }

  private def parseArguments(args: Array[String]) =
    Arguments.Parser('-')
      .withOption(opts.showPrivateDefs)
      .withOption(opts.verbose)
      .withOption(opts.version)
      .withOption(opts.help)
      .withOptionalArg(opts.classpath)
      .withOptionalArg(opts.cp)
      // TODO two temporary, hidden options to be able to test different classpath representations
      .withOption(opts.disableFlatClassPathCaching)
      .withOption(opts.logClassPath)
      .parse(args)

  private def createClassPath(cpArg: Option[String], settings: Settings) = cpArg match {
    case Some(cp) =>
      AggregateClassPath(new ClassPathFactory(settings).classesInExpandedPath(cp))
    case _ =>
      settings.classpath.value = "." // include '.' in the default classpath SI-6669
      new PathResolver(settings).result
  }
}
