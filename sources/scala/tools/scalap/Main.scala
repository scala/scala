/*     ___ ____ ___   __   ___   ___
**    / _// __// _ | / /  / _ | / _ \    Scala classfile decoder
**  __\ \/ /__/ __ |/ /__/ __ |/ ___/    (c) 2003, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_/_/
**
**  $Id$
*/

package scala.tools.scalap;

import java.io._;


/** The main object used to execute scalap on the command-line.
 *
 *  @author		Matthias Zenger
 *  @version 	1.0, 10/02/2004
 */
object Main {

    /** The version number.
     */
    val VERSION = "1.0.1";

    /** Verbose program run?
     */
    var verbose = false;

    /** Prints usage information for scalap.
     */
    def usage: Unit = {
        Console.println("usage: scalap {<option>} <name>");
        Console.println("where <option> is");
        Console.println("  -private               print private definitions");
        Console.println("  -verbose               print out additional information");
        Console.println("  -version               print out the version number of scalap");
        Console.println("  -help                  display this usage message");
        Console.println("  -classpath <pathlist>  specify where to find user class files");
        Console.println("  -cp <pathlist>         specify where to find user class files");
    }

    /** Executes scalap with the given arguments and classpath for the
     *  class denoted by 'classname'.
     */
    def process(args: Arguments, path: ClassPath)(classname: String): Unit = {
    	// find the classfile
		val file = path.openClass(Names.encode(
			if (classname == "scala.AnyRef") "java.lang.Object"
			else classname));
		// if the classfile exists...
		if (file.exists) {
			if (verbose)
				Console.println(Console.BOLD + "FILENAME" + Console.RESET +
				                " = " + file.getPath);
			// construct a reader for the classfile content
			val reader = new ByteArrayReader(file.content);
			// parse the classfile
			val clazz = new Classfile(reader);
			// check if there is a Scala signature attribute
			val attrib = clazz.attribs.find(a => a.toString() == "ScalaSignature");
			attrib match {
				// if the attribute is found, we have to extract the scope
				// from the attribute
				case Some(a) =>
					// parse the Scala attribute
					val info = new ScalaAttribute(a.reader);
					val symtab = new EntityTable(info);
					// construct a new output stream writer
					val out = new OutputStreamWriter(System.out);
					val writer = new ScalaWriter(args, out);
					// output all elements of the scope
					symtab.root.elements foreach (
						sym => { writer.printSymbol(sym);
								 writer.println*; });
					out.flush();
				// It must be a Java class
				case None =>
					// construct a new output stream writer
				    val out = new OutputStreamWriter(System.out);
				    val writer = new JavaWriter(clazz, out);
				    // print the class
				    writer.printClass;
				    out.flush();
			}
		// if the class corresponds to the artificial class scala.Any...
		} else if (classname == "scala.Any") {
			Console.println("package scala;");
			Console.println("class Any {");
			Console.println("    def eq(scala.Any): scala.Boolean;");
			Console.println("    final def ==(scala.Any): scala.Boolean;");
			Console.println("    final def !=(scala.Any): scala.Boolean;");
			Console.println("    def equals(scala.Any): scala.Boolean;");
			Console.println("    def hashCode(): scala.Int;");
			Console.println("    def toString(): java.lang.String;");
			Console.println("    final def isInstanceOf[T]: scala.Boolean;");
			Console.println("    final def asInstanceOf[T]: T;");
			Console.println("    def match[S, T](f: S => T): T;");
			Console.println("}");
		// explain that scala.All cannot be printed...
		} else if (classname == "scala.All") {
			Console.println("Type scala.All is artificial; " +
				"it is a subtype of all types.");
		// explain that scala.AllRef cannot be printed...
		} else if (classname == "scala.AllRef") {
			Console.println("Type scala.AllRef is artificial; it is a " +
				"subtype of all subtypes of scala.AnyRef.");
		// at this point we are sure that the classfile is not available...
		} else
			Console.println("class/object not found.");
    }

    /** The main method of this object.
     */
    def main(args: Array[String]) = {
    	// print usage information if there is no command-line argument
        if (args.length == 0)
            usage;
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
                Console.println("scalap " + VERSION);
            if (arguments contains "-help")
            	usage;
            verbose = arguments contains "-verbose";
            // construct a custom class path
            val path = arguments.getArgument("-classpath") match {
            	case None => arguments.getArgument("-cp") match {
            		case None => new ClassPath
            		case Some(path) => new ClassPath { override val classPath = path }
            	}
            	case Some(path) => new ClassPath { override val classPath = path }
            }
            // print the classpath if output is verbose
            if (verbose)
            	Console.println(Console.BOLD + "CLASSPATH" + Console.RESET +
            	                " = " + path);
			// process all given classes
           	arguments.getOthers.foreach(process(arguments, path));
        }
    }
}
