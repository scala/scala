
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

  /** Scala compiler adaptor. adapted from (see below for additions to Scala license)
  *  jaco.framework.ant.AntCompilerAdaptor (c) Matthias Zenger, and
  *  org.apache.tools.ant.taskdefs.DefaultCompilerAdapter
  *                          (c) the Apache Software Foundation
  *
  * @author  Burak Emir
  * @version 1.6
  * $Id$
  */

  class AntAdaptor extends DefaultCompilerAdapter {

    final val PRODUCT = System.getProperty("scala.product", "scalac");
    final val VERSION = System.getProperty("scala.version", "unknown version");

    def runCompiler( args:Array[String] ) = {
      var result = true;
      val reporter = new Reporter();
      val command = new CompilerCommand(PRODUCT,
                                        VERSION,
                                        reporter,
                                        new CompilerPhases());
      if( command.parse( args ) && command.files.list.size() > 0 ) {
        val global = new Global(command);
	try {
	  global.compile(command.files.toArray(), false);
	} catch {
          case e:Throwable => {
            /* e.printStackTrace(); */
            result = false;
            throw new BuildException(e.getMessage());
          }
        }
        global.stop("total");
        global.reporter.printSummary();
      }
        result;
    }

    def compilerName() = "scalac";

    override def setJavac( attributes:Javac ) = {
      super.setJavac( attributes );
    }

    def execute() = {
      attributes.log("Using " + compilerName() + " as scala compiler",
                     Project.MSG_VERBOSE);
      runCompiler( setupScalacCommand().getArguments() );
    }

    def setupScalacCommand() = {
      val cmd = new Commandline();

      if( destDir != null ) {
        cmd.createArgument().setValue("-d");
        cmd.createArgument().setFile(destDir);
      }

      this.includeJavaRuntime = true;

      if( compileClasspath != null ) {
        cmd.createArgument().setValue("-classpath");
        cmd.createArgument().setPath( getCompileClasspath() );
      }

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

      if (extdirs != null && extdirs.size() > 0) {
        cmd.createArgument().setValue("-extdirs");
        cmd.createArgument().setPath(extdirs);
      }

      logAndAddFilesToCompile(cmd);
      cmd
    }
  }
}

/*
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2000-2002 The Apache Software Foundation.  All rights
 * reserved.
 * Copyright (c) 2000-2002 Matthias Zenger.  All rights reserved.
 * Copyright (c) 2003-2004 Burak Emir.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "The Jakarta Project", "Ant", and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Group.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
*/
