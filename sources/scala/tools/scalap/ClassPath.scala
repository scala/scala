// ClassPath
// 04-Mar-2002, Matthias Zenger

package scalap;

import java.io._;
import scala.collection._;


class ClassPath {

    /** the character separating files
     */
    protected val FILE_SEP = File.separator;

    /** the separator in class path specifications
     */
    protected val PATH_SEP = System.getProperty("path.separator");

    /** the default class path
     */
    val classPath = System.getProperty("java.class.path");

    /** the default boot class path
     */
    val bootPath = System.getProperty("sun.boot.class.path");

    /** the default extension path
     */
    val extensionPath = System.getProperty("java.ext.dirs");

    /** the corresponding file cache (for not reading .jar files over
     *  and over again)
     */
	val cache = new FileCache;

    /** the various class path roots
     */
    protected var root: List[String] = decompose(bootPath) :::
                                       expand(extensionPath) :::
                                       decompose(classPath);


    /** append files from the extension directories
     */
    protected def expand(edirs: String): List[String] =
        if (edirs == null)
        	Nil
        else {
            val extdirs = edirs + PATH_SEP;
            val length = extdirs.length();
            var i = 0;
            var path: List[String] = Nil;
            while (i < length) {
                val k = extdirs.indexOf(PATH_SEP, i);
                val dirname = extdirs.substring(i, k);
                if ((dirname != null) && (dirname.length() > 0)) {
                    val iter = Iterator.fromArray(new File(dirname).list());
                    val dname = if (dirname.endsWith(FILE_SEP)) dirname else dirname + FILE_SEP;
                    while (iter.hasNext) {
                    	val entry = iter.next;
                    	if (entry.endsWith(".jar"))
                            path = (dname + entry) :: path;
                    }
                }
                i = k + 1;
            }
            path
        };

    /** parse a class path specification and return an array
     *  of existing class file locations
     */
    protected def decompose(p: String): List[String] = {
    	val path = if (p.endsWith(PATH_SEP)) p else p + PATH_SEP;
    	var components: List[String] = Nil;
        var i = 0;
        while (i < path.length()) {
            val j = path.indexOf(PATH_SEP, i);
            val subpath = path.substring(i, j);
            if (new File(subpath).exists())
            	components = subpath :: components;
            i = j + 1;
        }
        components.reverse;
    }

    /** find file with given name in class path and return an abstract
     *  file representation together with the originating class path
     *  component.
     */
    def findFile(name: String): Pair[AbstractFile, String] = {
      	val iter = root.elements;
      	var entry: AbstractFile = null;
      	var continue: Boolean = true;
      	var origin: String = null;
      	while (continue && iter.hasNext) {
      		origin = iter.next;
      		entry = cache.open(origin, name);
      		if (entry.exists)
      			continue = false;
      	}
      	Pair(entry, origin)
    }

    /** find file with given name in class path and return an abstract
     *  file representation.
     */
    def openFile(name: String): AbstractFile = findFile(name)._1;

    /** find class with given name in class path and return an abstract
     *  file representation.
     */
    def openClass(name: String): AbstractFile =
    	openFile(name.replace('.', File.separatorChar) + ".class");

    def elements: Iterator[String] = root.elements;

    def classes: Iterator[String] = new Iterator[String] {
    	val todo: mutable.Stack[Pair[AbstractFile, String]] = new mutable.Stack;
    	var iter: Iterator[String] = Iterator.empty;
    	var file: AbstractFile = null;
    	var path: String = null;
    	var clazz: String = null;
        root.foreach { component => {
        	val f = cache.open(component, null);
        	if (f.exists && f.isDirectory)
        		todo.push(Pair(f, null));
        }};
    	scan;
    	def hasNext = (clazz != null);
    	def next =
    		if (clazz == null)
    			error("no next element");
    		else {
    			val res = clazz; scan; res
    		};
    	def scan: Unit = {
    		if (!iter.hasNext) {
    			if (todo.isEmpty)
    				clazz = null;
    			else {
    				val Pair(f, p) = todo.top;
    				todo.pop;
    				iter = f.elements;
    				file = f;
    				path = if ((p != null) && p.endsWith("/"))
            			p.substring(0, p.length() - 1) else p;
            		scan;
            	}
    		} else {
    			var continue = true;
    			while (continue && iter.hasNext) {
    				val g = file.open(iter.next);
    				clazz = if (path == null) g.getName else path + "." + g.getName;
    				if (clazz.endsWith(".class")) {
    					clazz = clazz.substring(0, clazz.length() - 6);
    					continue = false;
    				} else if (g.exists && g.isDirectory)
    					todo.push(Pair(g, clazz));
    			}
    			if (continue)
    				scan;
    		}
    	}
    }

    /** return a textual representation of this class path
     */
    override def toString() = root match {
    	case Nil => ""
    	case x :: Nil => x
    	case x :: xs => xs.foldLeft(x)((s, e) => s + PATH_SEP + e);
    }
}
