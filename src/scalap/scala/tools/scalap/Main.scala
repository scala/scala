/*     ___ ____ ___   __   ___   ___
**    / _// __// _ | / /  / _ | / _ \  Scala classfile decoder
**  __\ \/ /__/ __ |/ /__/ __ |/ ___/  (c) 2003-2009, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_/_/      http://scala-lang.org/
**
*/

// $Id$

package scala.tools.scalap


import java.io.{File, PrintStream, OutputStreamWriter, ByteArrayOutputStream}
import scala.tools.nsc.util.ClassPath
import scalax.rules.scalasig._

/**The main object used to execute scalap on the command-line.
 *
 * @author Matthias Zenger, Stephane Micheloud, Burak Emir, Ilya Sergey
 */
object Main {
  val SCALA_SIG = "ScalaSig"
  val versionMsg = "Scala classfile decoder " +
          Properties.versionString + " -- " +
          Properties.copyrightString + "\n"

  /**Verbose program run?
   */
  var verbose = false
  var printPrivates = false

  /**Prints usage information for scalap.
   */
  def usage: Unit = {
    Console.println("usage: scalap {<option>} <name>")
    Console.println("where <option> is")
    Console.println("  -private           print private definitions")
    Console.println("  -verbose           print out additional information")
    Console.println("  -version           print out the version number of scalap")
    Console.println("  -help              display this usage message")
    Console.println("  -classpath <path>  specify where to find user class files")
    Console.println("  -cp <path>         specify where to find user class files")
  }

  def isScalaFile(bytes: Array[Byte]): Boolean = {
    val byteCode = ByteCode(bytes)
    val classFile = ClassFileParser.parse(byteCode)
    classFile.attribute("ScalaSig") match {case Some(_) => true; case None => false}
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
    writer.printClass
    out.flush()
  }

  def isPackageObjectFile(s: String) = s != null && (s.endsWith(File.separator + "package") || s == "package")

  def parseScalaSignature(scalaSig: ScalaSig, isPackageObject: Boolean) = {
    val baos = new ByteArrayOutputStream
    val stream = new PrintStream(baos)
    val syms = scalaSig.topLevelClasses ::: scalaSig.topLevelObjects
    syms.first.parent match {
    //Partial match
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
    for (c <- syms) {
      printer.printSymbol(c)
    }
    baos.toString
  }


  def decompileScala(bytes: Array[Byte], isPackageObject: Boolean) = {
    val byteCode = ByteCode(bytes)
    val classFile = ClassFileParser.parse(byteCode)
    classFile.attribute(SCALA_SIG).map(_.byteCode).map(ScalaSigAttributeParsers.parse) match {
      case Some(scalaSig) => Console.println(parseScalaSignature(scalaSig, isPackageObject))
      case None => //Do nothing
    }
  }


  /**Executes scalap with the given arguments and classpath for the
   *  class denoted by <code>classname</code>.
   *
   * @param args...
   * @param path...
   * @param classname...
   */
  def process(args: Arguments, path: ClassPath#Build)(classname: String): Unit = {
    // find the classfile
    val filename = Names.encode(
      if (classname == "scala.AnyRef") "java.lang.Object"
      else classname).replace('.', File.separatorChar)
    val cfile = path.lookupPath(filename, /*isDir*/ false)
    if (cfile != null) {
      if (verbose) {
        Console.println(Console.BOLD + "FILENAME" + Console.RESET + " = " + cfile.path)
      }
      val bytes = cfile.toByteArray
      if (isScalaFile(bytes)) {
        decompileScala(bytes, isPackageObjectFile(filename))
      } else {
        // construct a reader for the classfile content
        val reader = new ByteArrayReader(cfile.toByteArray)
        // parse the classfile
        val clazz = new Classfile(reader)
        processJavaClassFile(clazz)
      }
      // if the class corresponds to the artificial class scala.All.
      // (to be removed after update of the STARR libraries)
    } else if (classname == "scala.All") {
      Console.println("package scala")
      Console.println("/* Deprecated. Use scala.Nothing instead. */")
      Console.println("sealed abstract class All")
      // if the class corresponds to the artificial class scala.AllRef.
      // (to be removed after update of the STARR libraries)
    } else if (classname == "scala.AllRef") {
      Console.println("package scala")
      Console.println("/* Deprecated. Use scala.Null instead. */")
      Console.println("sealed abstract class AllRef")
      // if the class corresponds to the artificial class scala.Any.
      // (see member list in class scala.tool.nsc.symtab.Definitions)
    } else if (classname == "scala.Any") {
      Console.println("package scala")
      Console.println("class Any {")
      Console.println("  final def ==(scala.Any): scala.Boolean")
      Console.println("  final def !=(scala.Any): Boolean")
      Console.println("  def equals(scala.Any): scala.Boolean")
      Console.println("  def hashCode(): scala.Int")
      Console.println("  def toString(): java.lang.String")
      Console.println("  final def isInstanceOf[a]: scala.Boolean")
      Console.println("  final def asInstanceOf[a]: a")
      Console.println("}")
      // if the class corresponds to the artificial class scala.AnyRef.
    } else if (classname == "scala.AnyRef") {
      Console.println("package scala")
      Console.println("class AnyRef extends Any {")
      Console.println("  def equals(scala.Any): scala.Boolean")
      Console.println("  def hashCode(): scala.Int")
      Console.println("  def toString(): java.lang.String")
      Console.println("}")
      // if the class corresponds to the artificial class scala.AnyVal.
    } else if (classname == "scala.AnyVal") {
      Console.println("package scala")
      Console.println("sealed class AnyVal extends Any")
      // if the class corresponds to the artificial class scala.Boolean.
    } else if (classname == "scala.Boolean") {
      Console.println("package scala")
      Console.println("sealed abstract class Boolean extends AnyVal {")
      Console.println("  def &&(p: => scala.Boolean): scala.Boolean  // boolean and")
      Console.println("  def ||(p: => scala.Boolean): scala.Boolean  // boolean or")
      Console.println("  def & (x: scala.Boolean): scala.Boolean     // boolean strict and")
      Console.println("  def | (x: scala.Boolean): scala.Boolean     // boolean stric or")
      Console.println("  def ==(x: scala.Boolean): scala.Boolean     // boolean equality")
      Console.println("  def !=(x: scala.Boolean): scala.Boolean     // boolean inequality")
      Console.println("  def !: scala.Boolean                        // boolean negation")
      Console.println("}")
      // if the class corresponds to the artificial class scala.Int.
    } else if (classname == "scala.Int") {
      Console.println("package scala")
      Console.println("sealed abstract class Int extends AnyVal {")
      Console.println("  def ==(that: scala.Double): scala.Boolean")
      Console.println("  def ==(that: scala.Float): scala.Boolean")
      Console.println("  def ==(that: scala.Long): scala.Boolean")
      Console.println("  def ==(that: scala.Int): scala.Boolean")
      Console.println("  def ==(that: scala.Short): scala.Boolean")
      Console.println("  def ==(that: scala.Byte): scala.Boolean")
      Console.println("  def ==(that: scala.Char): scala.Boolean")
      Console.println("  /* analogous for !=, <, >, <=, >= */")
      Console.println
      Console.println("  def + (that: scala.Double): scala.Double // double addition")
      Console.println("  def + (that: scala.Float): scala.Float   // float addition")
      Console.println("  def + (that: scala.Long): scala.Long     // long addition")
      Console.println("  def + (that: scala.Int): scala.Int       // int addition")
      Console.println("  def + (that: scala.Short): scala.Int     // int addition")
      Console.println("  def + (that: scala.Byte): scala.Int      // int addition")
      Console.println("  def + (that: scala.Char): scala.Int      // int addition")
      Console.println("  /* analogous for -, *, /, % */")
      Console.println
      Console.println("  def & (that: scala.Long): scala.Long     // long bitwise and")
      Console.println("  def & (that: scala.Int): scala.Int       // int bitwise and")
      Console.println("  def & (that: scala.Short): scala.Int     // int bitwise and")
      Console.println("  def & (that: scala.Byte): scala.Int      // int bitwise and")
      Console.println("  def & (that: scala.Char): scala.Int      // int bitwise and")
      Console.println("  /* analogous for |, ^ */")
      Console.println
      Console.println("  def <<(cnt: scala.Int): scala.Int        // int left shift")
      Console.println("  def <<(cnt: scala.Long): scala.Int       // long left shift")
      Console.println("  /* analogous for >>, >>> */")
      Console.println
      Console.println("  def + : scala.Int                        // int identity")
      Console.println("  def - : scala.Int                        // int negation")
      Console.println("  def ~ : scala.Int                        // int bitwise negation")
      Console.println
      Console.println("  def toByte: scala.Byte                   // convert to Byte")
      Console.println("  def toShort: scala.Short                 // convert to Short")
      Console.println("  def toChar: scala.Char                   // convert to Char")
      Console.println("  def toInt: scala.Int                     // convert to Int")
      Console.println("  def toLong: scala.Long                   // convert to Long")
      Console.println("  def toFloat: scala.Float                 // convert to Float")
      Console.println("  def toDouble: scala.Double               // convert to Double")
      Console.println("}")
      // if the class corresponds to the artificial class scala.Nothing.
    } else if (classname == "scala.Nothing") {
      Console.println("package scala")
      Console.println("sealed abstract class Nothing")
      // if the class corresponds to the artificial class scala.Null.
    } else if (classname == "scala.Null") {
      Console.println("package scala")
      Console.println("sealed abstract class Null")
    } else
      Console.println("class/object " + classname + " not found.")
  }

  /**The main method of this object.
   */
  def main(args: Array[String]) {
    // print usage information if there is no command-line argument
    if (args.length == 0)
      usage
    // otherwise parse the arguments...
    else {
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
        usage
      verbose = arguments contains "-verbose"
      printPrivates = arguments contains "-private"
      // construct a custom class path
      val classPath0 = new ClassPath(false)
      val path = arguments.getArgument("-classpath") match {
        case None => arguments.getArgument("-cp") match {
          case None => new classPath0.Build()
          case Some(path) => new classPath0.Build(path)
        }
        case Some(path) => new classPath0.Build(path)
      }
      // print the classpath if output is verbose
      if (verbose) {
        Console.println(Console.BOLD + "CLASSPATH" + Console.RESET + " = " + path)
      }
      // process all given classes
      arguments.getOthers.foreach(process(arguments, path))
    }
  }
}
