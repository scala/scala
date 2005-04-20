package scala.tools.scala4ant;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.Javac;
import org.apache.tools.ant.util._;
import java.io.File;
/*import java.util._; */

/** Scala AntTask. adapted from (see below for additions to Scala license)
 *  jaco.framework.ant.AntCompilerTask  (c) Matthias Zenger, and
 *  org.apache.tools.ant.taskdefs.Javac (c) the Apache Software Foundation
 *
 * @author  Burak Emir
 * @version 1.6
 * $Id$
 */

class AntTask extends Javac {

  private val fileUtils:FileUtils  = FileUtils.newFileUtils();

  var force : boolean = false;
  var xmarkup : boolean = false;

  override def execute():unit = {

    try {

      Class.forName("ch.epfl.lamp.fjbg.JFieldOrMethod"); /* sanity check */

    } catch {
      case e:ClassNotFoundException =>
        Console.println("Cannot run scala4ant.\nIt seems fjbg.jar is not in your CLASSPATH.");
        throw new BuildException("Cannot run scala4ant.\nIt seems fjbg.jar is not in your CLASSPATH.");
    };

    try {

      Class.forName("scala.tools.scala4ant.AntAdaptor$class"); /* sanity check */

    } catch {
      case e:ClassNotFoundException =>
        throw new BuildException("Cannot run scala4ant.\nIt seems ant cannot load the AntAdaptor$class.\n Put tools.jar in your classpath.");
    };

    getProject().setProperty("build.compiler",
                             "scala.tools.scala4ant.AntAdaptor$class");
    super.execute();
  }

  def setForce( b:boolean ) = this.force = b;
  def getForce() = force;

  /** only for compatibility, has no effect */
  def setXmarkup( b:boolean ) = this.xmarkup = b;
  def getXmarkup() = xmarkup;

  override protected def scanDir(srcDir:File, destDir:File, files:Array[String] ):unit = {
    if( force ) {
      val newCompileList = new Array[File]( compileList.length + files.length );
      System.arraycopy(compileList, 0, newCompileList, 0, compileList.length);
      var j, i = 0;

      def handleFile( theFile:String ):unit = {
        /* this, because wile create anon-class -> access violation */
        if( theFile.endsWith( ".scala" )) {
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
      m.setFrom("*.scala");
      m.setTo("*.class");
      val sfs = new SourceFileScanner(this);
      handleNewFiles( sfs.restrictAsFiles( files, srcDir, destDir, m ));
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
