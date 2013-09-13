package scala.tools.partest

import scala.tools.nsc.util.stackTraceString

sealed abstract class TestState {
  def testFile: java.io.File
  def what: String
  def reason: String
  def transcript: Array[String]

  def isOk             = false
  def isSkipped        = false
  def testIdent        = testFile.testIdent
  def transcriptString = transcript mkString EOL

  def identAndReason = testIdent + reasonString
  def status         = s"$what - $identAndReason"
  def longStatus     = status + transcriptString
  def reasonString   = if (reason == "") "" else s"  [$reason]"

  def shortStatus    = if (isOk) "ok" else "!!"

  override def toString = status
}

object TestState {
  case class Uninitialized(testFile: java.io.File) extends TestState {
    def what = "uninitialized"
    def reason = what
    def transcript = Array.empty[String]
    override def shortStatus = "??"
  }
  case class Pass(testFile: java.io.File) extends TestState {
    def what = "pass"
    override def isOk = true
    def transcript: Array[String] = Array.empty[String]
    def reason = ""
  }
  case class Updated(testFile: java.io.File) extends TestState {
    def what = "updated"
    override def isOk = true
    def transcript: Array[String] = Array.empty[String]
    def reason = "updated check file"
    override def shortStatus = "++"
  }
  case class Skip(testFile: java.io.File, reason: String) extends TestState {
    def what = "skip"
    override def isOk = true
    override def isSkipped = true
    def transcript: Array[String] = Array.empty[String]
    override def shortStatus = "--"
  }
  case class Fail(testFile: java.io.File, reason: String, transcript: Array[String]) extends TestState {
    def what = "fail"
  }
  case class Crash(testFile: java.io.File, caught: Throwable, transcript: Array[String]) extends TestState {
    def what = "crash"
    def reason = s"caught $caught_s - ${caught.getMessage}"

    private def caught_s = (caught.getClass.getName split '.').last
    override def transcriptString = nljoin(super.transcriptString, caught_s)
  }
}
