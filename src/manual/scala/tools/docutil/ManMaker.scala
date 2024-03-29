/* NSC -- new Scala compiler
 * Copyright 2005-2018 LAMP/EPFL
 * Adapted from Lex Spoon's sbaz manual
 */

package scala.tools.docutil

import java.io.{File, FileOutputStream}

class ManMaker {

  /** The command for which to generate the man page */
  private var command: List[String] = Nil

  /** The directory to put html pages in */
  private var htmlout: Option[File] = None

  /** The directory to put man pages in */
  private var manout: Option[File] = None


  def setCommand(input: String): Unit = {
    command = input.split(",").toList.flatMap { s =>
      val st = s.trim()
      if (st != "") List(st) else Nil
    }
  }

  def setHtmlout(input: File): Unit = {
    htmlout = Some(input)
  }

  def setManout(input: File): Unit = {
    manout = Some(input)
  }

  def execute(): Unit = {
    if (command.isEmpty) sys.error("Attribute 'command' is not set.")
    if (htmlout.isEmpty) sys.error("Attribute 'htmlout' is not set.")
    if (manout.isEmpty) sys.error("Attribute 'manout' is not set.")

    command foreach (cmd => {
      val classname = "scala.man1."+ cmd

      val htmlFileName = htmlout.get.getPath + File.separator +
                         cmd + ".html"
      val htmlFile = new java.io.FileOutputStream(htmlFileName)
      EmitHtml.emitHtml(classname, htmlFile)

      val manFileName = manout.get.getPath + File.separator +
                        "man1" + File.separator + cmd + ".1"
      val manFile = new FileOutputStream(manFileName)
      EmitManPage.emitManPage(classname, manFile)
    })
  }
}

/** Command line runner for ManMaker which is called from the sbt build. */
object ManMaker extends App {
  val Array(commands, htmlout, manout) = args: @unchecked
  val mm = new ManMaker
  mm.setCommand(commands)
  mm.setHtmlout(new File(htmlout))
  mm.setManout(new File(manout))
  mm.execute()
}
