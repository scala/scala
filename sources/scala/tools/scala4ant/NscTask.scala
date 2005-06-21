/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2004, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
** $Id$
\*                                                                      */

package scala.tools.scala4ant;

import java.io.File;

import org.apache.tools.ant.taskdefs.Javac;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.util._;


/**
 * The <code>NscTask</code> class provides an Ant task for
 * for the as-yet nonexistent <code>nsc</code> command.
 * i.e.<pre>
 *  &lt;nsc srcdir="${src}" destdir="${build}"&gt;
 *      &lt;include name="test.scala"/&gt;
 *  &lt;/nsc&gt;
 * </pre>
 *
 * @author  Burak Emir, Stephane Micheloud
 * @version 1.0
 */

class NscTask extends Javac {

  private val fileUtils: FileUtils  = FileUtils.newFileUtils();

  private var verbose = false;
  private var force = false;
  private var nscArgs = "";

  def setCp(s: Path) = setClasspath(s);

  def setForce(b: Boolean) = this.force = b;
  def getForce() = force;

  def setNscArgs(s: String) = this.nscArgs = s;
  def getNscArgs() = nscArgs;

  def moreArgs():Array[String] = {
    getNscArgs().split(";");
  }

  override def execute() = {
    System.setProperty("scala.home", ScalaRuntime.home.toString());
    //System.setProperty("scala.product", scala.tools.scalac.Main.PRODUCT);
    //System.setProperty("scala.version", scala.tools.scalac.Main.VERSION);
    System.setProperty("scala.class.path", ".");
    System.setProperty("scala.boot.class.path", ScalaRuntime.bootclasspath.toString());

    getProject().setProperty("build.compiler",
                             "scala.tools.scala4ant.NscAdaptor$class");

    super.execute();
  }

  override protected def scanDir(srcDir: File, destDir: File, files: Array[String]): Unit = {
    if (force) {
      val newCompileList = new Array[File](compileList.length + files.length);
      System.arraycopy(compileList, 0, newCompileList, 0, compileList.length);
      var j, i = 0;

      def handleFile( theFile:String ):unit = {
        /* this, because wile create anon-class -> access violation */
        if (theFile.endsWith(".scala")) {
          if (srcDir == null) {
            newCompileList(compileList.length + j) = new File(theFile);
            j = j + 1;
          }
          else {
            newCompileList(compileList.length + j) =
              fileUtils.resolveFile(srcDir, theFile);
            j = j + 1
          }
        }
      }

      while (i < files.length) {
        handleFile(files(i));
        i = i + 1;
      }

      if (j == files.length)
        compileList = newCompileList;
      else {
        compileList = new Array[File](j);
        System.arraycopy(newCompileList, 0, compileList, 0, j);
      }
    } else {
      val m = new GlobPatternMapper();

      def handleNewFiles(newFiles:Array[File]): Unit = {
        if (newFiles.length > 0) {
          val newCompileList =
            new Array[File](compileList.length + newFiles.length);
          System.arraycopy(compileList, 0, newCompileList, 0,
                           compileList.length);
          System.arraycopy(newFiles, 0, newCompileList,
                           compileList.length, newFiles.length);
          compileList = newCompileList;
        }
      }
      m.setFrom("*.scala");
      m.setTo("*.class");
      val sfs = new SourceFileScanner(this);
      handleNewFiles(sfs.restrictAsFiles(files, srcDir, destDir, m));
    }
  }

}
