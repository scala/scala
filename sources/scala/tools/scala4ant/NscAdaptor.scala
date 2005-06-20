
//import scalac._;
import scala.tools.util.Reporter;

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

  class NscAdaptor extends AntAdaptor {

    override def runCompiler( args:Array[String] ) = {
      var ex:Throwable = _;
      //Console.println("runCompiler");
      var result = true;
      try {
        //var moreargs: List[String] = Nil;

        //if(verbose)
        //  moreargs = "-verbose" :: moreargs;

        val moreargs = this.attributes.asInstanceOf[NscTask].moreArgs();

        for(val a <- moreargs)
          Console.println("NscAdaptor adds argument '"+a+"'");

        val nargs = new Array[String](moreargs.length + args.length);
        //moreargs.copyToArray(nargs, 0);
        System.arraycopy(moreargs, 0, nargs, 0, moreargs.length);
        System.arraycopy(args, 0, nargs, moreargs.length, args.length);




        scala.tools.nsc.Main.process( nargs );
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

    override def compilerName() = "nsc";

  }
}

