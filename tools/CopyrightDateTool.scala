/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2016, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

import sys.process._

object CopyrightDateTool {
  val usage = """

    This tool compares a copyright date in files with the date of last change in git.
    If the message of last change contains "[Sync copyright dates]" it will be ignored and
    we will take next commit.

    analyze - just shows if anything is ok
    update - file fixing

    From root folder:
    Usage: scala tools/CopyrightDateTool.scala analyze
           scala tools/CopyrightDateTool.scala update

  """
  trait DateComparison {
    def fileLastUpdateYear: String
    def gitLastUpdateYear: String
    def fileCreationYear: String
    def gitCreationYear: String

    def isUpdateDateWrong(): Boolean = {
      fileLastUpdateYear.toInt != gitLastUpdateYear.toInt
    }

    def isUpdateDateWrongStr(): Boolean = {
      fileLastUpdateYear != gitLastUpdateYear
    }
  }

  case class FileDates(pathToFile: String,
                      fileLastUpdateYear: String,
                      gitLastUpdateYear: String,
                      fileCreationYear: String,
                      gitCreationYear: String) extends DateComparison

  case class GitInfo(message: String, date: String)

  def readFilesList(): Array[String] = {
    val git_files = Seq("/bin/sh", "-c", 
      "git grep --name-only --fixed-strings" + 
      " '|    (c) 20' -- '*.scala'").!!
    val files_array = git_files.split("\n")
    files_array
  }

  def readFile(pathToFile: String): String = {
    val source = scala.io.Source.fromFile(pathToFile)("UTF-8")
    val lines = try source.mkString finally source.close()
    lines
  }

  def getGitCreationYear(pathToFile: String): String = {
    val buf_dates = s"git log --format=%ci $pathToFile" !!
    val dates = buf_dates.split("\\n")
    val year_column = 0
    val res = dates.last.split("-")(year_column)
    res
  }

  def getGitLastUpdateYear(pathToFile: String): String = {
    val buf_dates = s"git log -3 --format=%cd $pathToFile" !!
    val buf_messages = s"git log -3 $pathToFile" !!
    val first_empty_elem = 1
    val messages = buf_messages.split("commit").drop(first_empty_elem)
    val dates = buf_dates.split("\\n")
    val gits_info = (messages, dates).zipped.map{ case(a,b) => GitInfo(a,b)}
    val res = gits_info.filterNot(_.message.contains("[Sync copyright dates]"))
    val year_column = 4
    val first = 0
    res(first).date.split(" ")(year_column)
  }

  def getYear(pathToFile: String, lower_bound: Int, upper_bound: Int): String = {
    val file = readFile(pathToFile)
    val third_line = 2
    val start_index = file.split("\\n")(third_line).indexOf("(c) 20")
    val year = file.split("\\n")(third_line)
                   .substring(start_index + lower_bound,
                              start_index + upper_bound)
    year
  }

  def getFileCreationYear(pathToFile: String): String = {
    val lb = "(c) ".length()
    val ub = "(c) 20XX".length()
    val fileCreationYear = getYear(pathToFile, lb, ub)
    fileCreationYear
  }

  def getFileLastUpdateYear(pathToFile: String): String = {
    val lb = "(c) 20XX-".length()
    val ub = "(c) 20XX-XXXX".length()
    val fileLastUpdateYear = getYear(pathToFile, lb, ub)
    fileLastUpdateYear
  }

  def analyzeDates(): Array[FileDates] = {
    val files_array = readFilesList()
    val data = files_array map {
      case file_path =>
      val fileLastUpdateYear = getFileLastUpdateYear(file_path)
      val gitLastUpdateYear = getGitLastUpdateYear(file_path)
      val fileCreationYear = getFileCreationYear(file_path)
      val gitCreationYear = getGitCreationYear(file_path)
      FileDates(file_path,
                fileLastUpdateYear,
                gitLastUpdateYear,
                fileCreationYear,
                gitCreationYear)
     }
    data
  }

  def fixWrongDates(): Boolean = {
    var check = false
    val files = analyzeDates()
    files.foreach { file =>
      val file_file = readFile(file.pathToFile)
      var new_file: Option[String] = None
      if (file.fileLastUpdateYear.forall(_.isDigit)) {
        if (file.isUpdateDateWrong()) {
          new_file =Some(file_file.
                       replace(
                        s"${file.fileCreationYear}-${file.fileLastUpdateYear}",
                        s"${file.fileCreationYear}-${file.gitLastUpdateYear}"
                       ))
        }
      }
      if(new_file.isDefined) {
        scala.tools.nsc.io.File(file.pathToFile)("UTF-8").writeAll(new_file.get)
        check = true 
      }
    }
    check
  }

  def resultAnalyzeDates(): Boolean = {
    val data = analyzeDates
    val (error_files, ok_files) = data.partition(x =>
      x.isUpdateDateWrongStr() &&
      x.fileLastUpdateYear.forall(_.isDigit))
    if (error_files.nonEmpty)
    {
      println("""
        Format
        FileDates(file_path,file update year,git update year, file creation year, git creation year)
        """)
      error_files.foreach { file => println(file) }
      println("Oops...We have some error dates")
    }
    else println ("Everything is ok")
    error_files.nonEmpty
  }

  def main(args: Array[String]) {
    val argList = args.toList
    argList match {
      case "analyze" :: Nil => resultAnalyzeDates()
      case "update" :: Nil => fixWrongDates()
      case _ => println(usage)
    }
  }
}
