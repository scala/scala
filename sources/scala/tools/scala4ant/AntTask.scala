package scala.tools.scala4ant;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.Javac;
import org.apache.tools.ant.util._;
import java.io.File;
import java.util._;


/** Scala AntTask.
 *
 * adapted from package jaco.framework.ant.AntCompilerTask
 *  (part of Matthias Zenger's jaco framework)
 *
 * @todo deduce path to scala sources using path to tools.jar (brittle)
 *
 * @author  Burak Emir
 * @version 1.5
 * $Id$
 */

class AntTask extends Javac {

    private var suffixes = "scala";
    private var force = false;
    private var source:String = null;
    private var mytarget:String = null;
    private val fileUtils:FileUtils  = FileUtils.newFileUtils();

    override def execute():unit = {
      try{
        Class.forName("ch.epfl.lamp.fjbg.JFieldOrMethod"); // simple check

        val project = getProject();
        //val old = project.getProperty("build.compiler");
        project.setProperty("build.compiler", "scala.tools.scala4ant.AntAdaptor$class");
        super.execute();
        //if (old == null)
        //     project.setProperty("build.compiler", "modern");
        //else
        //      project.setProperty("build.compiler", old);
      } catch {
        case e:ClassNotFoundException =>
          throw new BuildException("Cannot run scala4ant. It seems fjbg.jar is not in your CLASSPATH.");
      }

    }

    def setForce( fc:boolean ) = {
        force = fc;
    }

    def getForce() = {
        force;
    }

    override def setSource( source:String ) = {
        this.source = source;
    }

    override def getSource():java.lang.String = {
        source;
    }

    override def setTarget( target:String ) = {
        this.mytarget = target;
    }

    override def getTarget():java.lang.String = {
        mytarget;
    }

    def setSuffixes( s:String ) = {
        suffixes = s;
    }

    def getSuffixes() = {
        suffixes;
    }

    def setScalaClasspath( s:String ) = {
      System.setProperty("scala.class.path",s);
      {}
    }
    def getScalaClasspath() = {
	System.getProperty("scala.class.path");
    }
    def setScalaBootClasspath( s:String ) = {
	System.setProperty("scala.boot.class.path",s);
      {}
    }
    def getScalaBootClasspath() = {
	System.getProperty("scala.boot.class.path");
    }

    protected def parseSuffixes():Array[String] = {
        val st = new StringTokenizer(suffixes, " ,");
        val al = new ArrayList();
        while( st.hasMoreTokens() ) {
            al.add("." + st.nextToken());
        }
        al.toArray((new Array[String]( al.size() )).asInstanceOf[Array[java.lang.Object]]).asInstanceOf[Array[String]];
    }

    private def hasLegalSuffix( suffixes:Array[String], file:String ) = {
        var res = false;
        for ( val s <- new IterableArray( suffixes ).elements ) {
            res = res | file.endsWith( s );
        }
        res
    }

    override protected def scanDir( srcDir:File, destDir:File, files:Array[String] ):unit = {
        val sfx = parseSuffixes();
        if( force ) {
            val newCompileList = new Array[File]( compileList.length + files.length );
            System.arraycopy(compileList, 0, newCompileList, 0, compileList.length);
            var j, i = 0;

          def handleFile( theFile:String ):unit = { // this, because wile create anon-class -> access violation
                if( hasLegalSuffix(sfx, theFile )) {
                    if( srcDir == null ) {
                        newCompileList( compileList.length + j ) = new File( theFile );
                        j = j + 1;
                    } else {
                        newCompileList( compileList.length + j ) =
                            fileUtils.resolveFile( srcDir, theFile );
                        j = j + 1
                    }
                }
          }

          while( i < files.length ) {
            handleFile( files( i ) );
            i = i + 1;
          }

          if( j == files.length )
              compileList = newCompileList;
          else {
            compileList = new Array[File]( j );
            System.arraycopy(newCompileList, 0, compileList, 0, j);
          }
        } else {

          val m = new GlobPatternMapper();

          def handleNewFiles( newFiles:Array[File] ):unit = {
            if( newFiles.length > 0 ) {
              val newCompileList = new Array[ File ]( compileList.length +
                                                     newFiles.length);
              System.arraycopy(compileList, 0, newCompileList, 0,
                               compileList.length);
              System.arraycopy(newFiles, 0, newCompileList,
                               compileList.length, newFiles.length);
              compileList = newCompileList;
            }
          }

          for (val i <- scala.List.range( 0, sfx.length )) {
            m.setFrom("*" + sfx( i ));
            m.setTo("*.class");
            val sfs = new SourceFileScanner(this);
            handleNewFiles( sfs.restrictAsFiles(files, srcDir, destDir, m) );
          }

        }
    }
}
