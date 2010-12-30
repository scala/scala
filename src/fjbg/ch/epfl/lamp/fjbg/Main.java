/* FJBG -- Fast Java Bytecode Generator
 * Copyright 2002-2011 LAMP/EPFL
 * @author  Michel Schinz
 */

package ch.epfl.lamp.fjbg;

import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.jar.JarFile;
import java.util.zip.ZipEntry;

/**
 * Main program entry to execute the FJBG reader from the command line.
 *
 * The reader prints out the decoded data in the same output format as
 * javap, the Java bytecode disassembler of the Sun J2SE SDK.
 *
 * @author Stephane Micheloud
 * @version 1.1
 */

public class Main {
    private static final String PRODUCT_STRING = "Fast Java Bytecode Generator";
    private static final String VERSION_STRING = "version 1.1";

    private static final int ACTION_USAGE = 0;
    private static final int ACTION_DONE = 1;
    private static final int ACTION_PROCEED = 2;

    private static String classPath = ".";
    private static String[] classNames = null;

    public static void main(String[] args) {
        switch (parseArgs(args)) {
        case ACTION_USAGE: printUsage(); break;
        case ACTION_PROCEED: processClasses(); break;
        default:
        }
    }

    private static void processClasses() {
        FJBGContext fjbgContext = new FJBGContext(49, 0);
        if (classNames.length > 0)
            try {
                for (int i = 0; i < classNames.length; ++i)
                    processClass(fjbgContext, classNames[i]);
            } catch (IOException e) {
                System.err.println(e.getMessage());
            }
        else
            System.err.println(
                "No classes were specified on the command line.  Try -help.");
    }

    private static void processClass(FJBGContext fjbgContext, String className)
    throws IOException {
        InputStream in = getInputStream(className);
        JClass jclass = fjbgContext.JClass(new DataInputStream(in));
        System.out.println(jclass);
        in.close();
    }

    private static InputStream getInputStream(String className) throws IOException {
        String name = null;
        String[] paths = classPath.split(File.pathSeparator);
        for (int i = 0; i < paths.length; ++i) {
            File parent = new File(paths[i]);
            if (parent.isDirectory()) {
                name = className.replace('.', File.separatorChar)+".class";
                File f = new File(parent, name);
                if (f.isFile()) return new FileInputStream(f);
            } else if (paths[i].endsWith(".jar")) {
                JarFile f = new JarFile(parent);
                name = className.replace('.', '/')+".class";
                ZipEntry e = f.getEntry(name);
                if (e != null) return f.getInputStream(e);
            }
        }
        throw new IOException("ERROR:Could not find "+className);
    }

    private static int parseArgs(String[] args) {
        ArrayList/*<String>*/ classes = new ArrayList();
        String arg = null;
        int action = ACTION_USAGE;
        int i = 0, n = args.length;
        while (i < n) {
            arg = args[i];
            if (arg.equals("-classpath") && (i+1) < n) {
               classPath = args[i+1]; i += 2;
            } else if (arg.equals("-cp") && (i+1) < n) {
               classPath = args[i+1]; i += 2;
            } else if (arg.equals("-help")) {
               i = n+1;
            //} else if (arg.equals("-v")) {
            //   verbose = true; i += 1;
            } else if (arg.equals("-version")) {
               System.err.println(PRODUCT_STRING+" "+VERSION_STRING);
               action = ACTION_DONE; i = n+1;
            } else if (arg.startsWith("-")) {
               System.err.println("invalid flag: "+arg);
               i = n+1;
            } else {
               classes.add(arg); i += 1;
            }
        }
        if (i == n && i > 0) {
            classNames = (String[])classes.toArray(new String[classes.size()]);
            action = ACTION_PROCEED;
        }
        return action;
    }

    private static void printUsage() {
        System.out.println("Usage: fjbg <options> <classes>");
        System.out.println();
        System.out.println("where possible options include:");
        System.out.println("  -cp <path>           Specify where to find user class files");
        System.out.println("  -classpath <path>    Specify where to find user class files");
        System.out.println("  -help                Print a synopsis of standard options");
        System.out.println("  -version             Version information");
        System.out.println();
        System.exit(1);
    }
}

