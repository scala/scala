/*     ___ ____ ___   __   ___   ___
**    / _// __// _ | / /  / _ | / _ \  Scala classfile decoder
**  __\ \/ /__/ __ |/ /__/ __ |/ ___/  (c) 2003-2013, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_/_/      http://scala-lang.org/
**
*/

package scala.tools.scalap

import java.io.{ PrintStream, OutputStreamWriter, ByteArrayOutputStream }
import scala.reflect.NameTransformer
import scalax.rules.scalasig._
import scala.tools.nsc.util.{ ClassPath, JavaClassPath }
import scala.tools.util.PathResolver
import ClassPath.DefaultJavaContext
import scala.tools.nsc.io.{ PlainFile, AbstractFile }

/**The main object used to execute scalap on the command-line.
 *
 * @author Matthias Zenger, Stephane Micheloud, Burak Emir, Ilya Sergey
 */
class Main {
  val SCALA_SIG            = "ScalaSig"
  val SCALA_SIG_ANNOTATION = "Lscala/reflect/ScalaSignature;"
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
  def processJavaClassFile(clazz: Classfile) {
    // construct a new output stream writer
    val out = new OutputStreamWriter(Console.out)
    val writer = new JavaWriter(clazz, out)
    // print the class
    writer.printClass
    out.flush()
  }

  def isPackageObjectFile(s: String) = s != null && (s.endsWith(".package") || s == "package")

  def parseScalaSignature(scalaSig: ScalaSig, isPackageObject: Boolean) = {
    val baos   = new ByteArrayOutputStream
    val stream = new PrintStream(baos)
    val syms   = scalaSig.topLevelClasses ++ scalaSig.topLevelObjects

    syms.head.parent match {
      // Partial match
      case Some(p) if (p.name != "<empty>") => {
        val path = p.path
        if (!isPackageObject) {
          stream.print("package ");
          stream.print(path);
          stream.print("\n")
        } else {
          val i = path.lastIndexOf(".")
          if (i > 0) {
            stream.print("package ");
            stream.print(path.substring(0, i))
            stream.print("\n")
          }
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
  def process(args: Arguments, path: ClassPath[AbstractFile])(classname: String): Unit = {
    // find the classfile
    val encName = classname match {
      case "scala.AnyRef" => "java.lang.Object"
      case _ =>
        // we have to encode every fragment of a name separately, otherwise the NameTransformer
        // will encode using unicode escaping dot separators as well
        // we can afford allocations because this is not a performance critical code
        classname.split('.').map(NameTransformer.encode).mkString(".")
    }
    val cls = path.findClass(encName)
    if (cls.isDefined && cls.get.binary.isDefined) {
      val cfile = cls.get.binary.get
      if (verbose) {
        Console.println(Console.BOLD + "FILENAME" + Console.RESET + " = " + cfile.path)
      }
      val bytes = cfile.toByteArray
      if (isScalaFile(bytes)) {
        Console.println(decompileScala(bytes, isPackageObjectFile(encName)))
      } else {
        // construct a reader for the classfile content
        val reader = new ByteArrayReader(cfile.toByteArray)
        // parse the classfile
        val clazz = new Classfile(reader)
        processJavaClassFile(clazz)
      }
      // if the class corresponds to the artificial class scala.Any.
      // (see member list in class scala.tool.nsc.symtab.Definitions)
    }
    else
      Console.println("class/object " + classname + " not found.")
  }

  object EmptyClasspath extends ClassPath[AbstractFile] {
    /**
     * The short name of the package (without prefix)
     */
    def name              = ""
    def asURLs            = Nil
    def asClasspathString = ""

    val context     = DefaultJavaContext
    val classes     = IndexedSeq()
    val packages    = IndexedSeq()
    val sourcepaths = IndexedSeq()
  }
}

object Main extends Main {
  /** Prints usage information for scalap. */
  def usage() {
    Console println """
      |Usage: scalap {<option>} <name>
      |where <name> is fully-qualified class name or <package_name>.package for package objects
      |and <option> is
      |  -private           print private definitions
      |  -verbose           print out additional information
      |  -version           print out the version number of scalap
      |  -help              display this usage message
      |  -classpath <path>  specify where to find user class files
      |  -cp <path>         specify where to find user class files
    """.stripMargin.trim
  }

  def main(args: Array[String]) {
    // print usage information if there is no command-line argument
    if (args.isEmpty)
      return usage()

    val arguments = Arguments.Parser('-')
            .withOption("-private")
            .withOption("-verbose")
            .withOption("-version")
            .withOption("-help")
            .withOptionalArg("-classpath")
            .withOptionalArg("-cp")
            .parse(args);

    if (arguments contains "-version")
      Console.println(versionMsg)
    if (arguments contains "-help")
      usage()

    verbose       = arguments contains "-verbose"
    printPrivates = arguments contains "-private"
    // construct a custom class path
    val cparg = List("-classpath", "-cp") map (arguments getArgument _) reduceLeft (_ orElse _)
    val path = cparg match {
      case Some(cp) => new JavaClassPath(DefaultJavaContext.classesInExpandedPath(cp), DefaultJavaContext)
      case _        => PathResolver.fromPathString(".") // include '.' in the default classpath SI-6669
    }
    // print the classpath if output is verbose
    if (verbose)
      Console.println(Console.BOLD + "CLASSPATH" + Console.RESET + " = " + path)

    // process all given classes
    arguments.getOthers foreach process(arguments, path)
  }
}
