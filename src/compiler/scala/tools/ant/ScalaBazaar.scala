/*                     __                                               *\
**     ________ ___   / /  ___     Scala Ant Tasks                      **
**    / __/ __// _ | / /  / _ |    (c) 2005-2010, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.tools.ant {

  import scala.collection.DefaultMap
  import scala.collection.mutable.HashMap
  import java.io.{File, FileInputStream, FileOutputStream,
                  FileWriter, StringReader}
  import java.net.URL
  import java.util.ArrayList
  import java.util.zip.{ZipOutputStream, ZipEntry}

  import org.apache.tools.ant.{AntClassLoader, BuildException,
                               DirectoryScanner, Project}
  import org.apache.tools.ant.Task
  import org.apache.tools.ant.types.Path
  import org.apache.tools.ant.util.{FileUtils, MergingMapper,
                                    SourceFileScanner}
  import org.apache.tools.ant.types.{EnumeratedAttribute, Reference, FileSet}

  /** A set of files that can be installed at any relative location */
    class LooseFileSet {
      var destination: Option[String] = None
      def setDestination(dest: String) = {
        destination = Some(dest)
      }

      var fileset: Option[FileSet] = None
      def addConfiguredFileSet(fs: FileSet) = {
        fileset = Some(fs)
      }
    }
  /** An Ant task that generates a Scala Bazaars package (sbp file) along
    * with an advertisement of that package.
    *
    * This task can take the following parameters as attributes:<ul>
    *  <li>file (mandatory),</li>
    *  <li>adfile,</li>
    *  <li>name (mandatory),</li>
    *  <li>version (mandatory),</li>
    *  <li>depends,</li>
    *  <li>description,</li>
    *  <li>link.</li>
    *  </ul>
    *
    * @author Gilles Dubochet */
  class ScalaBazaar extends Task {

    /** The unique Ant file utilities instance to use in this task. */
    private val fileUtils = FileUtils.getFileUtils()

/******************************************************************************\
**                             Ant user-properties                            **
\******************************************************************************/

    /** The path to the archive file. */
    private var file: Option[File] = None
    /** The optional path to the advertisement file. */
    private var adfile: Option[File] = None
    /** The name of the package. */
    private var name: Option[String] = None
    /** The version number of the package. */
    private var version: Option[String] = None
    /** An (optional) list of names of the packages it depends of. */
    private var depends: List[String] = Nil
    /** An (optional) description of this package. */
    private var desc: Option[String] = None
    /** An (optional) URL link pointing to the location of the package */
    private var link: Option[String] = None

    /** The sets of files to include in the package */
    private object fileSetsMap extends DefaultMap[String, List[FileSet]] {
      private var content = new HashMap[String, List[FileSet]]()
      def get(key: String): Option[List[FileSet]] = content.get(key)
      override def size: Int = content.size
      def update(key: String, value: FileSet) {
        if (content.contains(key) && content(key) != Nil)
          content.update(key, value :: content(key))
        else content.update(key, List(value))
      }
      def fileSets = content.toList
      def iterator = content.iterator
    }



/******************************************************************************\
**                             Internal properties                            **
\******************************************************************************/


/******************************************************************************\
**                             Properties setters                             **
\******************************************************************************/

    /** Sets the file attribute. Used by Ant.
      * @param input The value of <code>file</code>. */
    def setFile(input: File) =
      file = Some(input)

    /** Sets the advertisement file attribute. Used by Ant.
      * @param input The value of <code>adfile</code>. */
    def setAdfile(input: File) =
      adfile = Some(input)

    /** Sets the name attribute of this package. Used by Ant.
      * @param input The value of <code>name</code>. */
    def setName(input: String) =
      name = Some(input)

    /** Sets the version attribute of this package. Used by Ant.
      * @param input The value of <code>version</code>. */
    def setVersion(input: String) =
      version = Some(input)

    /** Sets the depends attribute. Used by Ant.
      * @param input The value for <code>depends</code>. */
    def setDepends(input: String) = {
      depends = input.split(",").toList.flatMap { s: String =>
        val st = s.trim()
        (if (st != "") List(st) else Nil)
      }
    }

    /** Sets the description attribute of this package. Used by Ant.
      * @param input The value of <code>description</code>. */
    def setDesc(input: String) =
      desc = Some(input)

    /** Sets the link attribute of this package. Used by Ant.
      * @param input The value of <code>link</code>. */
    def setLink(input: String) =
      link = Some(input)

    def addConfiguredLibset(input: FileSet) =
      fileSetsMap.update("lib", input)

    def addConfiguredBinset(input: FileSet) =
      fileSetsMap.update("bin", input)

    def addConfiguredSrcset(input: FileSet) =
      fileSetsMap.update("src", input)

    def addConfiguredManset(input: FileSet) =
      fileSetsMap.update("man", input)

    def addConfiguredDocset(input: FileSet) =
      fileSetsMap.update("doc/" + getName, input)

    def addConfiguredMiscset(input: FileSet) =
      fileSetsMap.update("misc/" + getName, input)

    def addConfiguredLooseset(set: LooseFileSet) = {
      Pair(set.destination, set.fileset) match {
        case Pair(None, _) =>
          error("destination not specified for a loose file set")

        case Pair(_, None) =>
          error("no files specified for a loose file set")

        case Pair(Some(dest), Some(fileset)) =>
          fileSetsMap.update(dest, fileset)
      }
    }

/******************************************************************************\
**                             Properties getters                             **
\******************************************************************************/

    /** Gets the value of the file attribute in a Scala-friendly form.
      * @returns The file as a file. */
    private def getName: String =
      if (name.isEmpty) error("Name attribute must be defined first.")
      else name.get

    /** Gets the value of the file attribute in a Scala-friendly form.
      * @returns The file as a file. */
    private def getFile: File =
      if (file.isEmpty) error("Member 'file' is empty.")
      else getProject().resolveFile(file.get.toString())

    /** Gets the value of the adfile attribute in a Scala-friendly form.
      * @returns The adfile as a file. */
    private def getAdfile: File =
      if (adfile.isEmpty) error("Member 'adfile' is empty.")
      else getProject().resolveFile(adfile.get.toString())

/******************************************************************************\
**                       Compilation and support methods                      **
\******************************************************************************/
    /** Transforms a string name into a file relative to the provided base
      * directory.
      * @param base A file pointing to the location relative to which the name
      *   will be resolved.
      * @param name A relative or absolute path to the file as a string.
      * @return A file created from the name and the base file. */
    private def nameToFile(base: File)(name: String): File =
      existing(fileUtils.resolveFile(base, name))

    /** Transforms a string name into a file relative to the build root
      * directory.
      * @param name A relative or absolute path to the file as a string.
      * @return A file created from the name. */
    private def nameToFile(name: String): File =
      existing(getProject().resolveFile(name))

    /** Tests if a file exists and prints a warning in case it doesn't. Always
      * returns the file, even if it doesn't exist.
      * @param file A file to test for existance.
      * @return The same file. */
    private def existing(file: File): File = {
      if (!file.exists())
        log("Element '" + file.toString() + "' does not exist.",
            Project.MSG_WARN)
      file
    }

    /** Generates a build error. Error location will be the current task in the
      * ant file.
      * @param message A message describing the error.
      * @throws BuildException A build error exception thrown in every case. */
    private def error(message: String): Nothing =
      throw new BuildException(message, getLocation())

    private def writeFile(file: File, content: String) =
      if (file.exists() && !file.canWrite())
        error("File " + file + " is not writable")
      else {
        val writer = new FileWriter(file, false)
        writer.write(content)
        writer.close()
      }

/******************************************************************************\
**                           The big execute method                           **
\******************************************************************************/

    /** Performs the compilation. */
    override def execute() = {
      // Tests if all mandatory attributes are set and valid.
      if (file.isEmpty) error("Attribute 'file' is not set.")
      if (name.isEmpty) error("Attribute 'name' is not set.")
      if (version.isEmpty) error("Attribute 'version' is not set.")

      val pack = {
        <package>
          <name>{name.get}</name>
          <version>{version.get}</version>{
            if (!depends.isEmpty)
              <depends>{
                for (depend <- depends) yield
                  <name>{depend}</name>
              }</depends>
            else Nil
          }{
            if (!desc.isEmpty)
              <description>{desc.get}</description>
            else Nil
          }
        </package>
      }

      log("Creating package '" + name.get + "'")

      // Creates the advert file
      val advert = {
        <availablePackage>
          {pack}
          {link match {
            case None => <link>INSERT LINK HERE</link>
            case Some(str) => <link>{str}</link>
          }}
        </availablePackage>
      };

      if (!adfile.isEmpty)
        writeFile(getAdfile, advert.toString())

      // Checks for new files and creates the ZIP

      val zipContent =
        for {
          Pair(folder, fileSets) <- fileSetsMap.fileSets
          fileSet <- fileSets
          file <- fileSet.getDirectoryScanner(getProject).getIncludedFiles.toList
        } yield Triple(folder, fileSet.getDir(getProject), file)
      val zip = new ZipOutputStream(new FileOutputStream(file.get, false))
      if (!zipContent.isEmpty) {
        for (Triple(destFolder, srcFolder, file) <- zipContent) {
          log(file, Project.MSG_DEBUG)
          zip.putNextEntry(new ZipEntry(destFolder + "/" + file))
          val input = new FileInputStream(nameToFile(srcFolder)(file))
          val buf = new Array[Byte](10240)
          var n = input.read(buf, 0, buf.length)
          while (n >= 0) {
            zip.write (buf, 0, n)
            n = input.read(buf, 0, buf.length)
          }
          zip.closeEntry()
          input.close()
        }
      } else log("Archive contains no files.", Project.MSG_VERBOSE)
      zip.putNextEntry(new ZipEntry("meta/description"))
      val packInput = new StringReader(pack.toString())
      var byte = packInput.read()
      while (byte != -1) {
        zip.write (byte)
        byte = packInput.read()
      }
      zip.closeEntry()
      packInput.close()
      zip.close
    }

  }

}
