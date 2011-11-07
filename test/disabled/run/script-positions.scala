import scala.tools.nsc._
import util.stringFromStream

// Testing "scripts" without the platform delights which accompany actual scripts.
object Scripts {

  val test1 = 
"""#!/bin/sh 
  exec scala $0 $@ 
!#

println("statement 1")
println("statement 2".thisisborked)
println("statement 3")
"""

  val output1 =
"""thisisborked.scala:6: error: value thisisborked is not a member of java.lang.String
println("statement 2".thisisborked)
                      ^
one error found"""
  val test2 =
"""#!scala
// foo
// bar
!#

val x = "line 6"
val y = "line 7"
val z "line 8""""

  val output2 = 
"""bob.scala:8: error: '=' expected but string literal found.
val z "line 8"
      ^
bob.scala:8: error: illegal start of simple expression
val z "line 8"
             ^
two errors found"""
}

object Test {
  import Scripts._
  
  def settings = new GenericRunnerSettings(println _)
  settings.nocompdaemon.value = true
  
  def runScript(code: String): String =
    stringFromStream(stream =>
      Console.withOut(stream) {
        Console.withErr(stream) {
          ScriptRunner.runCommand(settings, code, Nil)
        }
      }
    )
  
  val tests: List[(String, String)] = List(
    test1 -> output1,
    test2 -> output2
  )
  // def lines(s: String) = s split """\r\n|\r|\n""" toList
  def lines(s: String) = s split "\\n" toList

  // strip the random temp filename from error msgs
  def stripFilename(s: String) = (s indexOf ".scala:") match {
    case -1   => s
    case idx  => s drop (idx + 7)
  }
  def toLines(text: String) = lines(text) map stripFilename
  
  def main(args: Array[String]): Unit = {
    for ((code, expected) <- tests) {      
      val out = toLines(runScript(code))
      val exp = toLines(expected)
      val nomatch = out zip exp filter { case (x, y) => x != y }
      val success = out.size == exp.size && nomatch.isEmpty
      
      assert(
        success,
        "Output doesn't match expected:\n" +
        "Expected:\n" + expected +
        "Actual:\n" + out.mkString("\n")
      )
    }
  }
}
