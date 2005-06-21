
//import scalac._;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.Execute;
import org.apache.tools.ant.taskdefs.Javac;
import org.apache.tools.ant.taskdefs.LogStreamHandler;
import org.apache.tools.ant.taskdefs.compilers.DefaultCompilerAdapter;
import org.apache.tools.ant.types.Commandline;
import org.apache.tools.ant.types.Path;
import java.io.IOException;

package scala.tools.scala4ant {

  /** Scala compiler adaptor. adapted from (see below for additions to Scala license)
  *  jaco.framework.ant.AntCompilerAdaptor (c) Matthias Zenger, and
  *  org.apache.tools.ant.taskdefs.DefaultCompilerAdapter
  *                          (c) the Apache Software Foundation
  *
  * @author  Burak Emir
  * @version 1.6
  * $Id$
  */

  class NscAdaptor extends DefaultCompilerAdapter {

     def runCompiler( args:Array[String] ) = {
      var ex:Throwable = _;
      //Console.println("runCompiler");
      var result = true;
      try {
        scala.tools.nsc.Main.process( args );
      } catch {
        case e:Throwable =>
          ex = e;
      }
      if( null != ex ) {
        ex.printStackTrace();
	    throw new BuildException("internal error of nsc:"+ex.getClass());
      }

      if( scala.tools.nsc.Main.errors() > 0 )
        throw new BuildException("there were compile errors");

	  true
    }

    def compilerName() = "nsc";

    def execute() = {
      attributes.log("Using " + compilerName() + " as scala compiler",
                     Project.MSG_VERBOSE);

      runCompiler( setupNscCommand().getArguments() );
    }

    def setupNscCommand() = {
      //Console.println("setupScalaCommand");
      val cmd = new Commandline();
      val cp = new Path( this.project );

      //- attribute @nscArgs in order to pass anything you like
      val moreargs = this.attributes.asInstanceOf[NscTask].moreArgs();
      for(val a <- moreargs) {
        Console.println("NscAdaptor adds argument '"+a+"'");
        cmd.createArgument().setValue(a);
      }


      //- encoding? @todo
      //val stask = this.attributes.asInstanceOf[ScalacTask];
      //if(null != stask.getEncoding()) {
      //  cmd.createArgument().setValue( "-encoding" );
      //  cmd.createArgument().setValue( stask.getEncoding() );
      //}

      //- destdir
      if( destDir != null ) {
        cmd.createArgument().setValue( "-d" );
        cmd.createArgument().setFile( destDir );
        cp.setLocation( destDir );
      }

      /* cp.addExisting( Path.systemClasspath );*/
      /* cp.addJavaRuntime(); */

      //- classpath
      if( compileClasspath != null ) {
        cp.addExisting( compileClasspath );
      }

      cmd.createArgument().setValue("-classpath");
      cmd.createArgument().setPath( cp );

      //var bcp = inferScalaPath(Path.systemClasspath.list());
      //val bcps = (bcp./: (PSEP) { (x:String,y:String) => x+PSEP+y });

      /* scala.tools.util.ClassPath replaces "::" / ";;" at the beginning
         with something meaningful
      */
      //cmd.createArgument().setValue("-bootclasspath");
      //cmd.createArgument().setValue( bcps );

      cmd.createArgument().setValue("-sourcepath");
      cmd.createArgument().setPath(
        if (compileSourcepath != null) {
          compileSourcepath;
        } else {
          src;
        }
      );

      if (bootclasspath != null && bootclasspath.size() > 0) {
        cmd.createArgument().setValue("-bootclasspath");
        cmd.createArgument().setPath(bootclasspath);
      }

      //if (extdirs != null && extdirs.size() > 0) {
      //  cmd.createArgument().setValue("-extdirs");
      //  cmd.createArgument().setPath(extdirs);
      //}

      logAndAddFilesToCompile(cmd);
      //Console.println("DONE setupScalaCommand");
      cmd
    }


  }
}

