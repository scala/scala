/*      ____ ____  ____ ____  ______                                     *\
 **    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
 **  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002-2005, LAMP/EPFL         **
 ** /_____/\____/\___/\____/____/                                        **
 **                                                                      **
\*                                                                       */

// $Id: $

package scala.tools.scalac.ant;

import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Vector;

import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DirectoryScanner;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.MatchingTask;
import org.apache.tools.ant.types.EnumeratedAttribute;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.util.FileUtils;
import org.apache.tools.ant.util.GlobPatternMapper;
import org.apache.tools.ant.util.SourceFileScanner;
import org.apache.tools.ant.types.Reference;

import scala.tools.scalac.CompilerPhases$class;
import scala.tools.scalac.Global$class;
import scala.tools.util.ConsoleReporter;
import scala.tools.util.Reporter;
import scala.tools.util.Timer;
import scala.tools.util.debug.AbortError;
import scalac.CompilationUnit;
import scalac.CompilerCommand;

/**
 * An Ant task to compile with the old Scala compiler.
 * This task can take the following parameters as attributes:<ul>
 *    <li>srcdir (mandatory),</li>
 *    <li>srcref,</li>
 *    <li>destdir,</li>
 *    <li>classpath,</li>
 *    <li>classpathref,</li>
 *    <li>sourcepath,</li>
 *    <li>sourcepathref,</li>
 *    <li>bootclasspath,</li>
 *    <li>bootclasspathref,</li>
 *    <li>extdirs,</li>
 *    <li>extdirsref,</li>
 *    <li>encoding,</li>
 *    <li>verbose,</li>
 *    <li>debug,</li>
 *    <li>usepredefs,</li>
 *    <li>useimports,</li>
 *    <li>force.</li>
 * </ul>
 * It also takes the following parameters as nested elements:<ul>
 *    <li>src (for srcdir),</li>
 *    <li>classpath,</li>
 *    <li>sourcepath,</li>
 *    <li>bootclasspath,</li>
 *    <li>extdirs.</li>
 * </ul>
 */
public class Scalac extends MatchingTask {

    private FileUtils fileUtils = FileUtils.newFileUtils();

    private String SCALA_PRODUCT =
    System.getProperty("scala.product", "Scalac Ant compiler");
    private String SCALA_VERSION =
    System.getProperty("scala.version", "Unknown version");

    // ###################################################################
    // #####                      Ant Properties                     #####
    // ###################################################################

    /** The directories that contain source files to compile. */
    private Path origin = null;
    /** The directory to put the compiled files in. */
    private File destination = null;

    /** The class path to use for this compilation. */
    private Path classpath = null;
    /** The source path to use for this compilation. */
    private Path sourcepath = null;
    /** The boot class path to use for this compilation. */
    private Path bootclasspath = null;
    /** The external extensions path to use for this compilation. */
    private Path extpath = null;

    /** The text encoding of the files to compile. */
    private String encoding = null;

    /** Whether to use verbose output or not. */
    private boolean verbose = false;
    /** Whether to print-out some additional ant compilation information. */
    private boolean debug = false;
    /** Whether to use implicit predefined values or not. */
    private boolean usepredefs = true;
    /** Whether to implicitly import or not. */
    private boolean useimports = true;
    /** What type of force compilation to use, if any. */
    private String force = "never";

    // ###################################################################
    // #####                    Properties setters                   #####
    // ###################################################################

    /**
     * A setter for the srcdir attribute. Used by Ant.
     * @param input The value of <code>origin</code>.
     */
    public void setSrcdir(Path input) {
        if (origin == null) {
            origin = input;
        } else {
            origin.append(input);
        }
    }

    /**
     * A setter of the <code>origin</code> as a nested src Ant parameter.
     * @return An origin path to be configured.
     */
    public Path createSrc() {
        if (origin == null) {
            origin = new Path(getProject());
        }
        return origin.createPath();
    }

    /**
     * A setter of the <code>origin</code> as an external reference Ant parameter.
     * @param input A reference to an origin path.
     */
    public void setSrcref(Reference input) {
        createSrc().setRefid(input);
    }

    /**
     * A setter for the destdir attribute. Used by Ant.
     * @param input The value of <code>destination</code>.
     */
    public void setDestdir(File input) {
        destination = input;
    }

    /**
     * A setter for the classpath attribute. Used by Ant.
     * @param input The value of <code>classpath</code>.
     */
    public void setClasspath(Path input) {
        if (classpath == null) {
            classpath = input;
        } else {
            classpath.append(input);
        }
    }

    /**
     * A setter of the <code>classpath</code> as a nested classpath Ant parameter.
     * @return A class path to be configured.
     */
    public Path createClasspath() {
        if (classpath == null) {
            classpath = new Path(getProject());
        }
        return classpath.createPath();
    }

    /**
     * A setter of the <code>classpath</code> as an external reference Ant parameter.
     * @param input A reference to a class path.
     */
    public void setClasspathref(Reference input) {
        createClasspath().setRefid(input);
    }

    /**
     * A setter for the sourcepath attribute. Used by Ant.
     * @param input The value of <code>sourcepath</code>.
     */
    public void setSourcepath(Path input) {
        if (sourcepath == null) {
            sourcepath = input;
        } else {
            sourcepath.append(input);
        }
    }

    /**
     * A setter of the <code>sourcepath</code> as a nested sourcepath Ant parameter.
     * @return A source path to be configured.
     */
    public Path createSourcepath() {
        if (sourcepath == null) {
            sourcepath = new Path(getProject());
        }
        return sourcepath.createPath();
    }

    /**
     * A setter of the <code>sourcepath</code> as an external reference Ant parameter.
     * @param input A reference to a source path.
     */
    public void setSourcepathref(Reference input) {
        createSourcepath().setRefid(input);
    }

    /**
     * A setter for the boot classpath attribute. Used by Ant.
     * @param input The value of <code>bootclasspath</code>.
     */
    public void setBootclasspath(Path input) {
        if (bootclasspath == null) {
            bootclasspath = input;
        } else {
            bootclasspath.append(input);
        }
    }

    /**
     * A setter of the <code>bootclasspath</code> as a nested sourcepath Ant parameter.
     * @return A source path to be configured.
     */
    public Path createBootclasspath() {
        if (bootclasspath == null) {
            bootclasspath = new Path(getProject());
        }
        return bootclasspath.createPath();
    }

    /**
     * A setter of the <code>bootclasspath</code> as an external reference Ant parameter.
     * @param input A reference to a source path.
     */
    public void setBootclasspathref(Reference input) {
        createBootclasspath().setRefid(input);
    }

    /**
     * A setter for the external extensions path attribute. Used by Ant.
     * @param input The value of <code>extpath</code>.
     */
    public void setExtdirs(Path input) {
        if (extpath == null) {
            extpath = input;
        } else {
            extpath.append(input);
        }
    }

    /**
     * A setter of the <code>extpath</code> as a nested sourcepath Ant parameter.
     * @return A source path to be configured.
     */
    public Path createExtdirs() {
        if (extpath == null) {
            extpath = new Path(getProject());
        }
        return extpath.createPath();
    }

    /**
     * A setter of the <code>extpath</code> as an external reference Ant parameter.
     * @param input A reference to a source path.
     */
    public void setExtdirsref(Reference input) {
        createExtdirs().setRefid(input);
    }

    /**
     * A setter for the encoding attribute. Used by Ant.
     * @param input The value of <code>encoding</code>.
     */
    public void setEncoding(String input) {
        encoding = input;
    }

    /**
     * A setter for the verbose attribute. Used by Ant.
     * @param input The value for <code>verbose</code>.
     */
    public void setVerbose(boolean input) {
        verbose = input;
    }

    /**
     * A setter for the debug attribute. Used by Ant.
     * @param input The value for <code>debug</code>.
     */
    public void setDebug(boolean input) {
        debug = input;
    }

    /**
     * A setter for the use predefs attribute. Used by Ant.
     * @param input The value for <code>usepredefs</code>.
     */
    public void setUsepredefs(boolean input) {
        usepredefs = input;
    }

    /**
     * A setter for the use imports attribute. Used by Ant.
     * @param input The value for <code>useimport</code>.
     */
    public void setUseimports(boolean input) {
        useimports = input;
    }

    /**
     * A setter for the force attribute. Used by Ant.
     * @param input The value for <code>force</code>.
     */
    public void setForce(ForceMode input) {
        force = input.getValue();
    }

    // ###################################################################
    // #####             Compilation and support methods             #####
    // ###################################################################

    /**
     * Generates a build error. Error location will be the current task in the ant file.
     *
     * @param  message The message of the error. This message should be end-user readable.
     * @throws org.apache.tools.ant.BuildException The build error exception.
     *         Will be thrown in all conditions.
     */
    private void error(String message) throws BuildException {
        throw new BuildException(message, getLocation());
    }

    /**
     * Performs the compilation.
     */
    public void execute() throws BuildException {

        // Tests if all mandatory attributes are set and valid.
        if (origin == null) error("Attribute 'srcdir' is not set.");
        if (origin.size() == 0) error("Attribute 'srcdir' is not set.");
        if (destination != null && !destination.isDirectory())
            error("Attribute 'destdir' does not refer to an existing directory.");

        Vector sourceFilesList = new Vector();

        // Scans source directories to build up a compile lists.
        // If force is false, only files were the .class file in destination
        // is newer than the .suffix file will be used.
        String[] originList = origin.list();
        for (int i = 0; i < originList.length; i++) {
            File originDir = getProject().resolveFile(originList[i]);
            if (!originDir.exists()) {
                log("Element '" + originDir.getPath() + "' in attribute 'srcdir' does not refer to an existing directory.", Project.MSG_WARN);
                break;
            }
            DirectoryScanner originDirScanner = this.getDirectoryScanner(originDir);
            String[] files = originDirScanner.getIncludedFiles();

            if (force.compareToIgnoreCase("always") == 0) {
                addFilesToSourceList(files, originDir, sourceFilesList);
            }
            else {
                GlobPatternMapper mapper = new GlobPatternMapper();
                mapper.setTo("*.class");
                mapper.setFrom("*.scala");
                SourceFileScanner scanner = new SourceFileScanner(this);
                String[] newFiles = scanner.restrict(files, originDir, destination, mapper);
                if (force.compareToIgnoreCase("changed") == 0 && (newFiles.length > 0)) {
                    addFilesToSourceList(files, originDir, sourceFilesList);
                }
                else if (force.compareToIgnoreCase("never") == 0) {
                    addFilesToSourceList(newFiles, originDir, sourceFilesList);
                }
            }
        }

        if (sourceFilesList.isEmpty()) {
            log("No files selected for compilation");
        }
        else {
            log("Compiling " + sourceFilesList.size() + " source file" + (sourceFilesList.size() == 1 ? "" : "s") + (destination != null ? " to " + destination.toString() : ""));
        }

        // Builds-up the compilation settings for Scalac with the existing Ant parameters.
        Reporter reporter = new ConsoleReporter();
        CompilerCommand command = new CompilerCommand(SCALA_PRODUCT, SCALA_VERSION, reporter, new CompilerPhases$class());
        if (destination != null) command.outpath.value = destination.getAbsolutePath();
        if (classpath != null) command.classpath.value = makeAbsolutePath(classpath, "classpath");
        if (sourcepath != null) {
            command.sourcepath.value = makeAbsolutePath(sourcepath, "sourcepath");
        }
        else {
            command.sourcepath.value = destination.getAbsolutePath();
        }
        // The bootclasspath needs to be treated specially.
        // When no bootclasspath is provided, the classpath of the current classloader is used:
        // This is where the scala classes should be.
        // Furthermore, the source files for the library must be available in the bootclasspath too.
        // Notice also how the bootclasspath must finish with a ":" for it to work.
        Path baseBootclasspath;
        if (bootclasspath != null) {
            baseBootclasspath = bootclasspath;
        }
        else {
            baseBootclasspath = getClassLoaderClasspath(this.getClass().getClassLoader());
        }
        command.bootclasspath.value =
            makeAbsolutePath(baseBootclasspath, "bootclasspath") + File.pathSeparator;
        if (!containsScala(command.bootclasspath.value)) {
            log("Bootclasspath does not contain a recognized Scala distribution. This might cause unexpected errors (Stack Overflow)", Project.MSG_WARN);
        }
        if (extpath != null) command.extdirs.value = makeAbsolutePath(extpath, "extpath");
        if (encoding != null) command.encoding.value = encoding;
        command.verbose.value = verbose;
        command.debug.value = debug;
        command.noimports.value = !useimports;
        command.nopredefs.value = !usepredefs;
        Timer timer = scalac.Global.getTimer(reporter);

        // Compiles the actual code
        Global$class compiler = new Global$class(command, timer, false);

        String[] stringArray = new String[sourceFilesList.size()];
        try {
            timer.start();
            CompilationUnit[] classes = compiler.compile((String[])sourceFilesList.toArray(stringArray), false);
            if (reporter.errors() > 0) {
                error("Compile failed with " + reporter.errors() + " error" + (reporter.errors() == 1 ? "" : "s") + "; see the compiler error output for details.");
            }
            compiler.dump(classes);
        }
        catch (AbortError exception) {
            if (debug) exception.printStackTrace();
            error("Compile failed because of an internal compiler error; see the error output for details.");
        }
        finally {
            timer.stop("Compile time");
        }
        if (reporter.warnings() > 0) {
            log("Compile suceeded with " + reporter.errors() + " warning" + (reporter.warnings() == 1 ? "" : "s") + "; see the compiler output for details.");
        }
    }

    private void addFilesToSourceList(String[] files, File originDir, Vector sourceFilesList) {
        for (int i = 0; i < files.length; i++) {
            String sourceFile = fileUtils.resolveFile(originDir, files[i]).toString();
            log(sourceFile, Project.MSG_VERBOSE);
            sourceFilesList.add(sourceFile);
        }
    }

    private String makeAbsolutePath(Path path, String pathName) {
        String result = "";
        String[] pathList = path.list();
        for (int i = 0; i < pathList.length; i++) {
            File pathFile = new File(pathList[i]);
            if (pathFile.exists()) {
                result = result + ((result == "") ? "" : File.pathSeparator) + pathFile.getAbsolutePath();
            }
            else {
                log("Element '" + pathFile.toString() + "' in " + pathName + " does not exist.", Project.MSG_WARN);
                result = result + ((result == "") ? "" : File.pathSeparator) + pathFile.toString();
            }
        }
        return result;
    }

    private Path getClassLoaderClasspath(ClassLoader classLoader) throws BuildException {
        Path classLoaderClasspath = new Path(getProject());
        ClassLoader parentClassLoader = classLoader.getParent();
        String classloaderName = classLoader.getClass().getName();
        boolean isURLClassLoader = classloaderName.endsWith("URLClassLoader");
        boolean isAntClassLoader = classloaderName.endsWith("AntClassLoader2") || classloaderName.endsWith("AntClassLoader");
        if (isURLClassLoader) {
            URL[] urls = ((URLClassLoader) classLoader).getURLs();
            for (int i = 0; i < urls.length; i++) {
                classLoaderClasspath.append(new Path(getProject(), urls[i].toString()));
            }
        }
        else if (isAntClassLoader) {
            String[] paths = ((AntClassLoader) classLoader).getClasspath().split(File.pathSeparator);
            for (int i = 0; i < paths.length; i++) {
                classLoaderClasspath.append(new Path(getProject(), paths[i]));
            }
        }
        if (parentClassLoader != null && parentClassLoader != classLoader) {
            classLoaderClasspath.append(getClassLoaderClasspath(parentClassLoader));
        }
        return classLoaderClasspath;
    }

    private boolean containsScala(String path) {
        boolean containsLibrary = false;
        boolean containsTools = false;
        String[] paths = path.split(File.pathSeparator);
        for (int i = 0; i < paths.length; i++) {
            if (paths[i].endsWith("scala.jar") || paths[i].endsWith("scala")) {
                containsLibrary = true;
            }
            if (paths[i].endsWith("tools.jar") || paths[i].endsWith("tools")) {
                containsTools = true;
            }
        }
        return containsLibrary && containsTools;
    }

    /**
     * Enumerated attribute with the values "never", "always", "changed".
     */
    public static class ForceMode extends EnumeratedAttribute {
        /**
         * @see EnumeratedAttribute#getValues
         */
        public String[] getValues() {
            return new String[] {"never", "always", "changed"};
        }
    }

}
