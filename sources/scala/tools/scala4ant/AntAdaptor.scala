
import scalac._;
import scalac.util.Reporter;

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

  /** a compiler adaptor for Scalac.
  *
  * author: Burak Emir
  * adapted from package jaco.framework.ant.AntCompilerAdaptor
  *  (part of Matthias Zenger's jaco framework)
 */

  class AntAdaptor extends DefaultCompilerAdapter {

    var source:String = _;
    var mytarget:String = _;

    final val PRODUCT = System.getProperty("scala.product", "scalac");
    final val VERSION = System.getProperty("scala.version", "unknown version");

    def runCompiler(args:Array[String]) = {
        var result = true;

	// dirty work to get rid of debugging (-g) option, set in setupJavac...

	val nargs = new Array[ String ]( args.length - 1 );
	var j = 0;
	for( val  i <- List.range( 0, args.length ) ) {
          Console.println( "args[ "+i+" ] = "+args(i));
	    if( !args( i ).startsWith("-g") ) {
		//System.err.print( args[ i ] +" ")
		nargs( j ) = args( i );
                j = j + 1;
	    }
        }

	// compile
        val reporter = new Reporter();
        val command = new CompilerCommand(PRODUCT, VERSION, reporter, new CompilerPhases());
        if( command.parse(nargs) && command.files.list.size() > 0 ) {
            val global = new Global(command);
	    try {
		global.compile(command.files.toArray(), false);
	    } catch {
                case e:Throwable => {
                    e.printStackTrace();
                    result = false;
                    throw new BuildException(e.getMessage());
                }
            }
            global.stop("total");
            global.reporter.printSummary();
	    /*
	      PizzaSettings js = new PizzaSettings();
	      js.parse(args);
	      return js.JavaContext().JavaCompiler().compile();
	    */
	}
	result;

    }

    def  compilerName() = "scalac";

    override def setJavac( attributes:Javac ) = {
      super.setJavac( attributes );
      val myattribs = attributes.asInstanceOf[AntTask];
      source = myattribs.getSource();
      mytarget = myattribs.getTarget();

    }

    def execute() = {
      attributes.log("Using " + compilerName() + " as scala compiler", Project.MSG_VERBOSE);
      runCompiler( setupScalacCommand().getArguments() );
    }

    def setupScalacCommand() = {
        val cmd = new Commandline();
        setupJavacCommandlineSwitches(cmd);
        //setupScalacCommandlineSwitches(cmd);
        logAndAddFilesToCompile(cmd);
        cmd
    }

    /*
    def setupScalacCommandlineSwitches( cmd:Commandline ) = {
        if (source != null) {
            cmd.createArgument().setValue("-source");
            cmd.createArgument().setValue(source);
        }
        if (target != null) {
            cmd.createArgument().setValue("-target");
            cmd.createArgument().setValue(target);
        }
    }
    */

  }
}
