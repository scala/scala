package scala.tools.scala4ant;

import scalac.*;
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


/** a compiler adaptor for Scalac.
 *
 * author: Burak Emir
 * adapted from package jaco.framework.ant.AntCompilerAdaptor
 *  (part of Matthias Zenger's jaco framework)
 */

public class AntAdaptor extends DefaultCompilerAdapter {

    private String source;
    private String target;

    public static final String PRODUCT =
        System.getProperty("scala.product", "scalac");
    public static final String VERSION =
        System.getProperty("scala.version", "unknown version");


    public boolean runCompiler(String[] args) {
	// dirty work, to get rid of debugging (-g) option, set in setupJavac...
	String[] nargs = new String[ args.length - 1 ];
	int j = 0;
	for( int i = 0; i<args.length; i++ )
	    if( !args[ i ].startsWith("-g") ) {
		//System.err.print( args[ i ] +" ")
		nargs[ j++ ] = args[ i ];
	    }
	// compile
        Reporter reporter = new Reporter();
        CompilerCommand command = new CompilerCommand(
            PRODUCT, VERSION, reporter, new CompilerPhases());
        if (command.parse(nargs) && command.files.list.size() > 0) {
            Global global = new Global(command);
	    try {
		global.compile(command.files.toArray(), false);
	    } catch (Throwable e) {
		e.printStackTrace();
		//throw new BuildException(e.message());
		return false;
	    }
            global.stop("total");
            global.reporter.printSummary();
	    /*
	      PizzaSettings js = new PizzaSettings();
	      js.parse(args);
	      return js.JavaContext().JavaCompiler().compile();
	    */
	}
	return true;

    }

    public String compilerName() {
        return "scalac";
    }

    public void setJavac(Javac attributes) {
        super.setJavac(attributes);
        AntTask myattribs = (AntTask)attributes;
        source = myattribs.getSource();
        target = myattribs.getTarget();
    }

    public boolean execute() throws BuildException {
        attributes.log("Using " + compilerName() + " as scala compiler",
                       Project.MSG_VERBOSE);
        return runCompiler(setupScalacCommand().getArguments());
    }

    public Commandline setupScalacCommand() {
        Commandline cmd = new Commandline();
        setupJavacCommandlineSwitches(cmd);
        //setupScalacCommandlineSwitches(cmd);
        logAndAddFilesToCompile(cmd);
        return cmd;
    }
    /*
    public void setupScalacCommandlineSwitches(Commandline cmd) {
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
