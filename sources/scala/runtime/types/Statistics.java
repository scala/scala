/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.runtime.types;

import java.io.PrintStream;
import java.io.FileOutputStream;
import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;

/**
 * Collect various statistics about run time types, and output them to
 * a file as s-expressions.
 *
 * Notice that all methods return true, in order to be usable as
 * assertions and disabled easily.
 *
 * @author Michel Schinz
 * @version 1.0
 */

public class Statistics {
    private static long instantiationsCount = 0;
    private static long uniqueInstantiationsCount = 0;

    private static long instanceOfCount = 0;
    private static long typeCastCount = 0;

    private static long ancestorSearchIterations = 0;
    private static long ancestorSearches = 0;

    private static HashMap instances = new HashMap();

    static {
        assert addWriteFileHook();
    }

    public static boolean addWriteFileHook() {
        Thread writeFileHook = new Thread() {
                public void run() {
                    try {
                        writeToFile();
                    } catch (Throwable t) {
                        throw new Error(t);
                    }
                }
            };

        Runtime.getRuntime().addShutdownHook(writeFileHook);
        return true;
    }

    public static synchronized boolean incInstantiations(boolean unique) {
        ++instantiationsCount;
        if (unique) ++uniqueInstantiationsCount;
        return true;
    }

    public static synchronized boolean incInstanceOf() {
        ++instanceOfCount;
        return true;
    }

    public static synchronized boolean decInstanceOf() {
        --instanceOfCount;
        return true;
    }

    public static synchronized boolean incTypeCast() {
        ++typeCastCount;
        return true;
    }

    public static synchronized boolean incInstances(String className) {
        Integer currCount = (Integer)instances.get(className);
        if (currCount == null)
            instances.put(className, new Integer(1));
        else
            instances.put(className, new Integer(currCount.intValue() + 1));
        return true;
    }

    public static synchronized boolean addAncestorSearchIterations(int n) {
        ancestorSearchIterations += n;
        ancestorSearches++;
        return true;
    }

    /**
     * Output statistics to a file, as an a-list associating numbers
     * to tags.
     */
    public static boolean writeToFile() throws java.io.FileNotFoundException {
        String fileName = System.getProperty("scala.runtime.types.statfile");
        assert fileName != null;

        System.out.println("Writing RTT statistics to file " + fileName);

        PrintStream stream = new PrintStream(new FileOutputStream(fileName));
        stream.println("(");
        stream.println("(instantiations . "
                       + instantiationsCount + ")");
        stream.println("(unique-instantiations . "
                       + uniqueInstantiationsCount + ")");
        stream.println("(instance-of . "
                       + instanceOfCount + ")");
        stream.println("(type-cast . "
                       + typeCastCount + ")");
        if (ancestorSearches > 0) {
            stream.println("(ancestor-searches . "
                           + ancestorSearches + ")");
            stream.println("(ancestor-search-iterations . "
                           + ancestorSearchIterations + ")");
        }
        stream.println("(instances . (");
        Iterator instIt = instances.entrySet().iterator();
        while (instIt.hasNext()) {
            Map.Entry entry = (Map.Entry)instIt.next();
            stream.println("(\"" + entry.getKey() + "\" . "
                           + entry.getValue() + ")");
        }
        stream.print("))");
        stream.println(")");
        stream.close();
        return true;
    }
}
