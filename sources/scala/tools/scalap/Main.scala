// ClassPath
// 04-Mar-2002, Matthias Zenger

package scalap;

import java.io._;
import scala.collection._;


object Main {

	def usage: Unit = {
	    Console.println("usage: scalap <name>");
	}

	def main(args: Array[String]) = {
	    if (args.length == 0)
	    	usage;
	    else {
			val path = new ClassPath;
			val file = path.openClass(Names.encode(args(0)));
			if (file.exists) {
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
	}
}
