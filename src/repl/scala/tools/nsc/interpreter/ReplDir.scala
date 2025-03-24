/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package interpreter

import java.io.PrintWriter

import io.VirtualDirectory
import settings.MutableSettings
import scala.reflect.io.{AbstractFile, Directory, PlainDirectory}
import scala.collection.mutable.Clearable

/** Directory to save .class files to. */
trait ReplDir extends AbstractFile with Clearable { }

private class ReplVirtualDir() extends VirtualDirectory("(memory)", None) with ReplDir { }
private class ReplRealDir(dir: Directory) extends PlainDirectory(dir) with ReplDir {
  def clear() = {
    dir.deleteRecursively()
    dir.createDirectory()
  }
}

class ReplOutput(val dirSetting: MutableSettings#StringSetting) {
  // outdir for generated classfiles - may be in-memory (the default),
  // a generated temporary directory, or a specified outdir.
  val dir: ReplDir = (
    if (dirSetting.isDefault)
      new ReplVirtualDir()
    else if (dirSetting.value == "")
      new ReplRealDir(Directory.makeTemp("repl"))
    else
      new ReplRealDir(Directory(dirSetting.value))
  )

  // print the contents hierarchically
  def show(out: PrintWriter) = {
    def pp(root: AbstractFile, indentLevel: Int): Unit = {
      val label = root.name
      val spaces = "    " * indentLevel
      out.println(spaces + label)
      if (root.isDirectory)
        root.toList sortBy (_.name) foreach (x => pp(x, indentLevel + 1))
    }
    pp(dir, 0)
  }
}
