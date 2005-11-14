/*    __  ______________                                                      *\
**   /  |/ / ____/ ____/                                                      **
**  / | | /___  / /___                                                        **
** /_/|__/_____/_____/ Copyright 2005 LAMP/EPFL                               **
\*                                                                            */

// $Id$

import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Vector;

import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DirectoryScanner;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.MatchingTask;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.util.FileUtils;
import org.apache.tools.ant.util.GlobPatternMapper;
import org.apache.tools.ant.util.SourceFileScanner;
import org.apache.tools.ant.types.Reference;
import org.apache.tools.ant.types.EnumeratedAttribute;

import scala.tools.nsc.reporters.{Reporter,ConsoleReporter};


package scala.tools.nsc.ant {

    /**
     * An Ant task to compile with the new Scala compiler (NSC).
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
     *    <li>force,</li>
     *    <li>stop,</li>
     *    <li>skip,</li>
     *    <li>check,</li>
     *    <li>showicode.</li>
     *    <li>log,</li>
     * </ul>
     * It also takes the following parameters as nested elements:<ul>
     *    <li>src (for srcdir),</li>
     *    <li>classpath,</li>
     *    <li>sourcepath,</li>
     *    <li>bootclasspath,</li>
     *    <li>extdirs.</li>
     * </ul>
     *
     * @author Gilles Dubochet
     */
    class NSC extends MatchingTask {

      private val SCALA_PRODUCT: String =
        System.getProperty("scala.product", "Scalac Ant compiler");
      private val SCALA_VERSION: String =
        System.getProperty("scala.version", "Unknown version");

      /** The unique Ant file utilities instance to use in this task. */
      private val fileUtils = FileUtils.newFileUtils();

      // ###################################################################
      // #####                      Ant Properties                     #####
      // ###################################################################

      abstract class PermissibleValue {
        val values: List[String];
        def isPermissible (value: String): Boolean =
          (value == "") ||
          values.exists(v: String => v startsWith value);
      }

      /** Defines valid values for the logging property. */
      object LoggingLevel extends PermissibleValue {
        val values = List("none", "verbose", "debug");
      }

      /** Defines valid values for properties that refer to compiler phases. */
      object CompilerPhase extends PermissibleValue {
        val values = List(
          "namer", "typer", "pickler", "uncurry", "tailcalls",
          "transmatch", "explicitouter", "erasure", "lambdalift",
          "flatten", "constructors", "mixin", "icode", "jvm", "terminal");
      }

      /** The directories that contain source files to compile. */
      private var origin: Option[Path] = None;
      /** The directory to put the compiled files in. */
      private var destination: Option[File] = None;

      /** The class path to use for this compilation. */
      private var classpath: Option[Path] = None;
      /** The source path to use for this compilation. */
      private var sourcepath: Option[Path] = None;
      /** The boot class path to use for this compilation. */
      private var bootclasspath: Option[Path] = None;
      /** The external extensions path to use for this compilation. */
      private var extpath: Option[Path] = None;

      /** The text encoding of the files to compile. */
      private var encoding: Option[String] = None;

      /** How much logging output to print. Either none (default), verbose or debug. */
      private var logging: Option[String] = None;
      /** Whether to use implicit predefined values or not. */
      private var usepredefs: Boolean = true;
      /** Whether to implicitly import or not. */
      private var useimports: Boolean = true;
      /** Whether to force compilation of all files or not. */
      private var force: Boolean = false;
      /** After which phase the compilation should stop. */
      private var stop: Option[String] = None;
      /** Which compilation phases should be skipped during compilation. */
      private var skip: List[String] = Nil;
      /** Which compilation phases should be logged during compilation. */
      private var logPhase: List[String] = Nil;
      /** Which compilation phases results should be checked for consistency. */
      private var check: List[String] = Nil;
      /** Print ICode files along with class files (debug option). */
      private var showICode: Boolean = false;

      // ###################################################################
      // #####                    Properties setters                   #####
      // ###################################################################

      /**
       * Sets the srcdir attribute. Used by Ant.
       * @param input The value of <code>origin</code>.
       */
      def setSrcdir (input: Path) =
        if (origin.isEmpty)
          origin = Some(input);
        else
          origin.get.append(input);

      /**
       * Sets the <code>origin</code> as a nested src Ant parameter.
       * @return An origin path to be configured.
       */
      def createSrc (): Path = {
        if (origin.isEmpty) {
          origin = Some(new Path(getProject()))
        }
        origin.get.createPath()
      }

      /**
       * Sets the <code>origin</code> as an external reference Ant parameter.
       * @param input A reference to an origin path.
       */
      def setSrcref (input: Reference) =
        createSrc().setRefid(input);

      /**
       * Gets the value of the origin attribute in a Scala-friendly form.
       * @returns The origin path as a list of files.
       */
      private def getOrigin: List[File] =
        if (origin.isEmpty)
          throw new ArrayIndexOutOfBoundsException("Member 'origin' is empty.");
        else
          List.fromArray(origin.get.list()).map(nameToFile("src"));

      /**
       * Sets the destdir attribute. Used by Ant.
       * @param input The value of <code>destination</code>.
       */
      def setDestdir (input: File) =
        destination = Some(input);

      /**
       * Gets the value of the destination attribute in a Scala-friendly form.
       * @returns The destination as a file.
       */
      private def getDestination: File =
        if (destination.isEmpty)
          throw new ArrayIndexOutOfBoundsException("Member 'destination' is empty.");
        else
          testReadableFile("destdir")(getProject().resolveFile(destination.get.toString()));

      /**
       * Sets the classpath attribute. Used by Ant.
       * @param input The value of <code>classpath</code>.
       */
      def setClasspath (input: Path) =
        if (classpath.isEmpty)
          classpath = Some(input);
        else
          classpath.get.append(input);

      /**
       * Sets the <code>classpath</code> as a nested classpath Ant parameter.
       * @return A class path to be configured.
       */
      def createClasspath (): Path = {
        if (classpath.isEmpty) {
          classpath = Some(new Path(getProject()))
        }
        classpath.get.createPath()
      }

      /**
       * Sets the <code>classpath</code> as an external reference Ant parameter.
       * @param input A reference to a class path.
       */
      def setClasspathref (input: Reference) =
        createClasspath().setRefid(input);

      /**
       * Gets the value of the classpath attribute in a Scala-friendly form.
       * @returns The class path as a list of files.
       */
      private def getClasspath: List[File] =
        if (classpath.isEmpty)
          throw new ArrayIndexOutOfBoundsException("Member 'classpath' is empty.");
        else
          List.fromArray(classpath.get.list()).map(nameToFile("classpath"));

      /**
       * Sets the sourcepath attribute. Used by Ant.
       * @param input The value of <code>sourcepath</code>.
       */
      def setSourcepath (input: Path) =
        if (sourcepath.isEmpty)
          sourcepath = Some(input);
        else
          sourcepath.get.append(input);

      /**
       * Sets the <code>sourcepath</code> as a nested sourcepath Ant parameter.
       * @return A source path to be configured.
       */
      def createSourcepath (): Path = {
        if (sourcepath.isEmpty) {
          sourcepath = Some(new Path(getProject()))
        }
        sourcepath.get.createPath()
      }

      /**
       * Sets the <code>sourcepath</code> as an external reference Ant parameter.
       * @param input A reference to a source path.
       */
      def setSourcepathref (input: Reference) =
        createSourcepath().setRefid(input);

      /**
       * Gets the value of the sourcepath attribute in a Scala-friendly form.
       * @returns The source path as a list of files.
       */
      private def getSourcepath: List[File] =
        if (sourcepath.isEmpty)
          throw new ArrayIndexOutOfBoundsException("Member 'sourcepath' is empty.");
        else
          List.fromArray(sourcepath.get.list()).map(nameToFile("sourcepath"));

      /**
       * Sets the boot classpath attribute. Used by Ant.
       * @param input The value of <code>bootclasspath</code>.
       */
      def setBootclasspath (input: Path) =
        if (bootclasspath.isEmpty)
          bootclasspath = Some(input);
        else
          bootclasspath.get.append(input);

      /**
       * Sets the <code>bootclasspath</code> as a nested sourcepath Ant parameter.
       * @return A source path to be configured.
       */
      def createBootclasspath (): Path = {
        if (bootclasspath.isEmpty) {
          bootclasspath = Some(new Path(getProject()))
        }
        bootclasspath.get.createPath()
      }

      /**
       * Sets the <code>bootclasspath</code> as an external reference Ant parameter.
       * @param input A reference to a source path.
       */
      def setBootclasspathref (input: Reference) =
        createBootclasspath().setRefid(input);

      /**
       * Gets the value of the bootclasspath attribute in a Scala-friendly form.
       * @returns The boot class path as a list of files.
       */
      private def getBootclasspath: List[File] =
        if (bootclasspath.isEmpty)
          throw new ArrayIndexOutOfBoundsException("Member 'bootclasspath' is empty.");
        else
          List.fromArray(bootclasspath.get.list()).map(nameToFile("bootclasspath"));

      /**
       * Sets the external extensions path attribute. Used by Ant.
       * @param input The value of <code>extpath</code>.
       */
      def setExtdirs (input: Path) =
        if (extpath.isEmpty)
          extpath = Some(input);
        else
          extpath.get.append(input);

      /**
       * Sets the <code>extpath</code> as a nested sourcepath Ant parameter.
       * @return An extensions path to be configured.
       */
      def createExtdirs (): Path = {
        if (extpath.isEmpty) {
          extpath = Some(new Path(getProject()))
        }
        extpath.get.createPath()
      }

      /**
       * Sets the <code>extpath</code> as an external reference Ant parameter.
       * @param input A reference to an extensions path.
       */
      def setExtdirsref (input: Reference) =
        createExtdirs().setRefid(input);

      /**
       * Gets the value of the extpath attribute in a Scala-friendly form.
       * @returns The extensions path as a list of files.
       */
      private def getExtpath: List[File] =
        if (extpath.isEmpty)
          throw new ArrayIndexOutOfBoundsException("Member 'extdirs' is empty.");
        else
          List.fromArray(extpath.get.list()).map(nameToFile("extdirs"));

      /**
       * Sets the encoding attribute. Used by Ant.
       * @param input The value of <code>encoding</code>.
       */
      def setEncoding(input: String): Unit =
        encoding = Some(input);

      /**
       * Sets the logging level attribute. Used by Ant.
       * @param input The value for <code>logging</code>.
       */
      def setLogging (input: String) =
        if (LoggingLevel.isPermissible(input))
          logging = Some(input);
        else
          error("Logging level '" + input + "' does not exist.");

      /**
       * Sets the use predefs attribute. Used by Ant.
       * @param input The value for <code>usepredefs</code>.
       */
      def setUsepredefs (input: Boolean): Unit =
        usepredefs = input;

      /**
       * Sets the use imports attribute. Used by Ant.
       * @param input The value for <code>useimport</code>.
       */
      def setUseimports (input: Boolean): Unit =
        useimports = input;

      /**
       * Sets the force attribute. Used by Ant.
       * @param input The value for <code>force</code>.
       */
      def setForce (input: Boolean): Unit =
        force = input;

      /**
       * Sets the force attribute. Used by Ant.
       * @param input The value for <code>force</code>.
       */
      def setStop (input: String) =
        if (CompilerPhase.isPermissible(input)) {
          if (input != "")
            stop = Some(input);
        }
        else
          error("Phase '" + input + "' in stop does not exist.");

      /**
       * Sets the force attribute. Used by Ant.
       * @param input The value for <code>force</code>.
       */
      def setSkip (input: String) = {
        skip = List.fromArray(input.split(",")).flatMap(s: String => {
          val st = s.trim();
          if (CompilerPhase.isPermissible(st)) (if (input != "") List(st) else Nil)
          else {error("Phase '" + st + "' in skip does not exist."); Nil}
        });
      }

      /**
       * Sets the log attribute. Used by Ant.
       * @param input The value for <code>logPhase</code>.
       */
      def setLog (input: String) = {
        logPhase = List.fromArray(input.split(",")).flatMap(s: String => {
          val st = s.trim();
          if (CompilerPhase.isPermissible(st)) (if (input != "") List(st) else Nil)
          else {error("Phase " + st + " in log does not exist."); Nil}
        });
      }

      /**
       * Sets the force attribute. Used by Ant.
       * @param input The value for <code>force</code>.
       */
      def setCheck (input: String) = {
        check = List.fromArray(input.split(",")).flatMap(s: String => {
          val st = s.trim();
          if (CompilerPhase.isPermissible(st)) (if (input != "") List(st) else Nil)
          else {error("Phase " + st + " in check does not exist."); Nil}
        });
      }

      def setShowicode(input: Boolean): Unit =
        showICode = input;

      // ###################################################################
      // #####             Compilation and support methods             #####
      // ###################################################################

      /**
       * Creates a file from a given string.
       * @param test A method to test whether the file is valid.
       * @param name The path of the file as a string.
       * @return The file corresponding to the provided name.
       */
      private def nameToFile(test: File=>File) (name: String): File =
        test(getProject().resolveFile(name));

      /**
       * Creates a file from a given string.
       * @param test A method to test whether the file is valid.
       * @param name The path of the file as a string.
       * @return The file corresponding to the provided name.
       */
      private def nameToFile (test: File=>File, origin: File) (name: String): File =
        test(fileUtils.resolveFile(origin, name));

        /**
         * Creates a file from a given string and tests its validity using the <code>testReadableFile</code> method.
         * @param pathName The name of the path in which the file is.
         * @param name The path of the file as a string.
         * @return The file corresponding to the provided name.
         */
        private def nameToFile (pathName: String, origin: File) (name: String): File = {
            nameToFile(testReadableFile(pathName), origin)(name);
        }

        /**
         * Creates a file from a given string and tests its validity using the <code>testReadableFile</code> method.
         * @param pathName The name of the path in which the file is.
         * @param name The path of the file as a string.
         * @return The file corresponding to the provided name.
         */
        private def nameToFile (pathName: String) (name: String): File = {
            nameToFile(testReadableFile(pathName))(name);
        }

        /**
         * Tests whether a file is readable (if it does not exist, it is not readable.
         * If it is not readable, prints a warning message.
         * @param pathName The name of the path in which the file is (used for printing-out warning message).
         * @param file The file to test.
         * @return The same file as provided.
         */
        private def testReadableFile (pathName: String)(file: File): File = {
            if (!file.exists())
                log("Element '" + file.toString() + "' in " + pathName + " does not exist.", Project.MSG_WARN);
            file
        }

        private def asString (path: List[File]): String = {
            path.map(file:File=>asString(file)).mkString("", ":", "")
        }

        private def asString (file: File): String = {
            file.getAbsolutePath()
        }

        /**
         * Generates a build error. Error location will be the current task in the ant file.
         * @param message The message of the error. This message should be end-user readable.
         * @throws org.apache.tools.ant.BuildException The build error exception. Will be thrown in all conditions.
         */
        private def error (message: String) = {
            throw new BuildException(message, getLocation());
        }

        private def getClassLoaderClasspath (classLoader: ClassLoader): List[File] = {
            val parentClassLoader = classLoader.getParent();
            val classloaderName = classLoader.getClass().getName();
            (if (parentClassLoader != null && parentClassLoader != classLoader)
                getClassLoaderClasspath(parentClassLoader)
            else Nil) :::
            (if (classloaderName.endsWith("URLClassLoader"))
                List.fromArray((classLoader.asInstanceOf[URLClassLoader]).getURLs()).map(url:URL=>new File(url.getFile()))
            else if (classloaderName.endsWith("AntClassLoader2") || classloaderName.endsWith("AntClassLoader"))
                List.fromArray(( classLoader.asInstanceOf[AntClassLoader]).getClasspath().split(File.pathSeparator)).map(nameToFile(f:File=>f))
            else Nil)
        }

        /**
         * Performs the compilation.
         */
        override def execute () = {

            // Tests if all mandatory attributes are set and valid.
            if (origin.isEmpty) error("Attribute 'srcdir' is not set.");
            if (getOrigin.isEmpty) error("Attribute 'srcdir' is not set.");
            if (!destination.isEmpty && !destination.get.isDirectory())
                error("Attribute 'destdir' does not refer to an existing directory.");
            if (destination.isEmpty) {
                destination = Some(getOrigin.head);
            }

            val mapper = new GlobPatternMapper();
            mapper.setTo("*.symbl");
            mapper.setFrom("*.scala");

            // Scans source directories to build up a compile lists.
            // If force is false, only files were the .class file in destination is older than
            // the .scala file will be used.
            val sourceFiles: List[File] =
              for (val originDir <- getOrigin;
                val originFile <- {
                  var includedFiles = getDirectoryScanner(originDir).getIncludedFiles();
                  if (!force) {
                    includedFiles = new SourceFileScanner(this)
                      .restrict(includedFiles, originDir, destination.get, mapper)
                  }
                  (List.fromArray(includedFiles)).map(nameToFile("srcdir", originDir))
                }
              ) yield {
                log(originFile.toString(), Project.MSG_VERBOSE);
                originFile
              }

            if (sourceFiles.length == 0)
            	log("No files selected for compilation")
            else
            	log("Compiling " + sourceFiles.length + " source file"
                                 + (if (sourceFiles.length > 1) "s" else "")
                                 + (" to " + getDestination.toString()));

            System.setProperty("scala.library.class.path", "");
            System.setProperty("scala.library.source.path", "");

            // Builds-up the compilation settings for Scalac with the existing Ant parameters.
            val reporter = new ConsoleReporter();
            val settings = new Settings(error);
            settings.outdir.value = asString(destination.get);
            if (!classpath.isEmpty) settings.classpath.value = asString(getClasspath);
            if (!sourcepath.isEmpty) settings.sourcepath.value = asString(getSourcepath);
            if (!bootclasspath.isEmpty) settings.bootclasspath.value = asString(getBootclasspath);
            if (!extpath.isEmpty) settings.extdirs.value = asString(getExtpath);
            if (!encoding.isEmpty) settings.encoding.value = encoding.get;
            if (!logging.isEmpty && logging.get == "verbose") {
              settings.verbose.value = true;
            }
            else if (!logging.isEmpty && logging.get == "debug") {
              settings.verbose.value = true;
              settings.debug.value = true;
            }
            settings.noimports.value = !useimports;
            settings.nopredefs.value = !usepredefs;
            if (!stop.isEmpty) settings.stop.value = List(stop.get);
            if (!skip.isEmpty) settings.skip.value = skip;
            if (!check.isEmpty) settings.check.value = check;
            settings.Xshowicode.value = showICode;
            if (!logPhase.isEmpty) settings.log.value = logPhase;

            // Sets path properties to prevent ClassPath from being corrupted.
            // It this isn't done, classpath will contain more than
            //System.setProperty("scala.library.class.path", "");
            //System.setProperty("scala.library.source.path", "");

            // Compiles the actual code
            val compiler = new Global(settings, reporter);
            try {
              (new compiler.Run).compile(sourceFiles.map(f:File=>f.toString()));
              if (reporter.errors() > 0)
                error("Compile failed with "
                      + reporter.errors() + " error"
                      + (if (reporter.errors() > 1) "s" else "")
                      + "; see the compiler error output for details.");
            }
            catch {
              case exception @ FatalError(msg) => {
		exception.printStackTrace();
                if (settings.debug.value) exception.printStackTrace();
                error("Compile failed because of an internal compiler error ("
                      + msg + "); see the error output for details.");
              }
            }
            if (reporter.warnings() > 0)
              log("Compile suceeded with "
                  + reporter.errors() + " warning"
                  + (if (reporter.warnings() > 1) "s" else "")
                  + "; see the compiler output for details.");
            reporter.printSummary()
        }

    }

}
