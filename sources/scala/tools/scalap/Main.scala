/*     ___ ____ ___   __   ___   ___
**    / _// __// _ | / /  / _ | / _ \    Scala classfile decoder
**  __\ \/ /__/ __ |/ /__/ __ |/ ___/    (c) 2003, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_/_/
**
**  $Id$
*/

package scala.tools.scalap;

import java.io._;
import scala.collection._;


object Main {

    val VERSION = "1.0a";

    var verbose = false;

    def usage: Unit = {
        Console.println("usage: scalap {<option>} <name>");
        Console.println("where <option> is");
        Console.println("  -verbose               print out additional information");
        Console.println("  -version               print out the version number of scalap");
        Console.println("  -help                  display this usage message");
        Console.println("  -classpath <pathlist>  specify where to find user class files");
        Console.println("  -cp <pathlist>         specify where to find user class files");
    }

    def process(path: ClassPath)(classname: String): Unit = {
		val file = path.openClass(Names.encode(classname));
		if (file.exists) {
			if (verbose)
				Console.println(Console.BOLD + "FILENAME" + Console.RESET +
				                " = " + file.getPath);
			val reader = new ByteArrayReader(file.content);
			val clazz = new Classfile(reader);
			val attrib = clazz.attribs.find(a => a.toString() == "ScalaSignature");
			attrib match {
				case Some(a) =>
					val info = new ScalaAttribute(a.reader);
					//Console.println("read attribute");
					val symtab = new EntityTable(info);
					//Console.println("read entities");
					//symtab.print;
					val out = new OutputStreamWriter(System.out);
					val writer = new ScalaWriter(out);
					symtab.root.elements foreach (
						sym => { writer.printSymbol(sym);
								 writer.println*; });
					out.flush();
				case None =>
					Console.println("Java classes not supported yet.");
			}
		} else
			Console.println("class/object not found.");
    }

    def main(args: Array[String]) = {
        if (args.length == 0)
            usage;
        else {
        	val arguments = Arguments.Parser('-')
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
            val path = arguments.getArgument("-classpath") match {
            	case None => arguments.getArgument("-cp") match {
            		case None => new ClassPath
            		case Some(path) => new ClassPath { override val classPath = path }
            	}
            	case Some(path) => new ClassPath { override val classPath = path }
            }
            if (verbose)
            	Console.println(Console.BOLD + "CLASSPATH" + Console.RESET +
            	                " = " + path);
           	arguments.getOthers.foreach(process(path));
        }
    }
}
