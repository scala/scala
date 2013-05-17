#!/bin/sh
exec scala -feature $0 $@
!#

import sys.process._

val tag1 = "v2.10.0-M4"
val tag2 = "v2.10.0-M5"

// Git commit parsing magikz

case class Commit(sha: String, author: String, header: String, body: String) {
  override def toString = " * " + sha + " (" + author + ") " + header + " - " + body.take(5) + " ..."
}

val gitFormat = "--format=format:*-*%h``%aN``%s``%b"

def processGitCommits(input: String): IndexedSeq[Commit] =
  ((input split "[\\r\\n]*\\*\\-\\*").view map (_ split "``") collect {
    case Array(sha, author, hdr, msg) => Commit(sha, author, hdr, msg)
  }).toVector

val commits =
  processGitCommits(Process(Seq("git", "log", tag1+".."+tag2,"--format=format:*-*%h``%aN``%s``%b","--no-merges")).!!)

val authors: Seq[(String, Int)] = {
  val grouped: Vector[(String,Int)] = (commits groupBy (_.author)).map { case (a,c) => a -> c.length }{collection.breakOut}
  (grouped sortBy (_._2)).reverse
}

def hasFixins(msg: String): Boolean = (
  (msg contains "SI-") /*&& ((msg.toLowerCase contains "fix") || (msg.toLowerCase contains "close"))*/
)

val fixCommits =
   for {
     commit <- commits
     searchString = commit.body + commit.header
     if hasFixins(searchString)
   } yield commit


val siPattern = java.util.regex.Pattern.compile("(SI-[0-9]+)")

def fixLinks(commit: Commit): String = {
  val searchString = commit.body + commit.header
  val m = siPattern matcher searchString
  val issues = new collection.mutable.ArrayBuffer[String]
  while(m.find()) {
    issues += (m group 1)
  }
  issues map (si => """<a href="https://issues.scala-lang.org/browse/%s">%s</a>""" format (si, si)) mkString ", "
}


// HTML Generation for Toni

def commitShaLink(sha: String) = 
  """<a href="https://github.com/scala/scala/commit/%s">%s</a>""" format (sha,sha)

def printBlankLine(): Unit = println("<p>&nbsp</p>")
def printHeader4(msg: String): Unit = println("<h4>%s</h4>" format (msg))

def printCommiterList(): Unit = {
  printBlankLine()
  printHeader4("Special thanks to all the contribtuors!")
  println("""<table border="0" cellspacing="0" cellpadding="1">
   <thead><tr><th>#</th><th align="left">Author</th></tr></thead>
   <tbody>""")
  for((author, count) <- authors)
     println("""<tr><td align="right">%d &nbsp;</td><td>%s</td></tr>""" format (count, author))
  println("""</tbody>
</table>""")
}

def printCommitList(): Unit = {
  printBlankLine()
  printHeader4("Complete commit list!")
  println("""<table border="0" cellspacing="0" cellpadding="1">
   <thead><tr><th>sha</th><th align="left">Title</th></tr></thead>
   <tbody>""")
  for(commit <- commits) {
     println("<tr>")
     println("""<td align="right">%s&nbsp;</td><td>%s</td>""" format (commitShaLink(commit.sha), commit.header))
     /*print("<td>")
     (commit.body split "[\\r\\n]") foreach { line =>
       print(line)
       print("<br/>")
     }
     print("</td>")*/
     println("""</tr>""")
  }
  println("""</tbody>
</table>""")
}

def issueFixPrinter(): Unit = {
  printBlankLine()
  printHeader4("Here's a list of isssues that have been fixed since %s" format (tag1))
  println("""<table border="0" cellspacing="0" cellpading="1">
  <thead><tr><th>Issue(s)</th><th>Commit</th><th>Message</th></tr></thead>
  <tbody>""")
  for(commit <- fixCommits) {
    println("""<tr><td>%s&nbsp;</td><td>%s&nbsp;</td><td>%s</td></tr>""" format(fixLinks(commit), commitShaLink(commit.sha), commit.header))
  }
  println("""</tbody>
</table>""")
   printBlankLine()
}

def printHTML(): Unit = {
  println("""<html>
  <head>
    <title>%s - Release notes</title>
  </head>
  <body>
  <h3>A new release of Scala is available!  Please point your build tools at %s</h3>
  <p>:: INSERT HAND GENERATED NOTES HERE ::</p>
""" format(tag2, tag2 drop 1))
  issueFixPrinter()
  printCommiterList()
  printCommitList()
  println("""</body></html>""")
}

printHTML()



