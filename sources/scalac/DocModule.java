/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

package scalac;

import scalac.Global;
import scalac.util.Debug;
import scalac.util.Reporter;
import ch.epfl.lamp.util.Position;
import java.net.URL;
import java.net.URLClassLoader;
import java.io.File;
import java.util.StringTokenizer;
import java.util.List;
import java.util.LinkedList;
import java.lang.reflect.Method;

/**
 * This class is used to dynamically load a documentation module.
 */
public class DocModule {

    /** Name of the method to be called (convention).
     */
    protected static final String METHOD_NAME = "apply";

    /** Complete the given path with standard class paths.
     */
    public static String completePath(String path) {
	String completePath = path;
	String path1 = System.getProperty("env.class.path");
	String path2 = System.getProperty("java.class.path");
	if (path1 != null)
	    completePath += File.pathSeparator + path1;
	if (path2 != null)
	    completePath += File.pathSeparator + path2;
	return completePath;
    }

    /** Get class loader from a given path.
     */
    public static ClassLoader getClassLoader(String path) {
        List urlList = new LinkedList();
	StringTokenizer st = new StringTokenizer(path, File.pathSeparator);
	try {
	    while (st.hasMoreTokens())
		urlList.add(new File(st.nextToken()).toURI().toURL());
	} catch(java.net.MalformedURLException e) {
	    throw Debug.abort(e);
	}
	URL[] urls = (URL[]) urlList.toArray(new URL[urlList.size()]);
	return new URLClassLoader(urls);
    }

    /** Get a class from its name.
     */
    public static Class getClass(String className, ClassLoader classLoader) {
	Class cls = null;
        try {
	    cls = classLoader.loadClass(className);
	} catch (ClassNotFoundException exc) {
	    throw Debug.abort("Class not found", className);
        }
	return cls;
    }

    /** Get the method
     */
    public static Method getMethod(String methodName, Class cls) {
	Method meth = null;
        try {
            meth = cls.getMethod(methodName,
				 new Class[] { Class.forName("scalac.Global") });
	} catch (Exception e) {
	    throw Debug.abort(e);
	}
	return meth;
    }

    public static void apply(Global global) {
	// complete path
	String path = completePath(global.docmodulePath);
	// class loader
	ClassLoader classLoader = getClassLoader(global.docmodulePath);
	// module class
	Class cls = getClass(global.docmodule, classLoader);
	// method
	Method meth = getMethod(METHOD_NAME, cls);
	// call method
	Thread.currentThread().setContextClassLoader(classLoader);
	try {
	    meth.invoke(null, new Object[] { global });
	} catch(Exception e) {
	    throw Debug.abort(e);
	}
    }
}
