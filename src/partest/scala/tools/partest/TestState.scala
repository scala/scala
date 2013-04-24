package scala.tools.partest

import scala.tools.nsc.FatalError
import scala.tools.nsc.util.stackTraceString

sealed abstract class TestState {
  def testFile: File
  def what: String
  def reason: String
  def transcript: List[String]

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
  case class Uninitialized(testFile: File) extends TestState {
    def what = "uninitialized"
    def reason = what
    def transcript = Nil
    override def shortStatus = "??"
  }
  case class Pass(testFile: File) extends TestState {
    def what = "pass"
    override def isOk = true
    def transcript: List[String] = Nil
    def reason = ""
  }
  case class Updated(testFile: File) extends TestState {
    def what = "updated"
    override def isOk = true
    def transcript: List[String] = Nil
    def reason = "updated check file"
    override def shortStatus = "++"
  }
  case class Skip(testFile: File, reason: String) extends TestState {
    def what = "skip"
    override def isOk = true
    override def isSkipped = true
    def transcript: List[String] = Nil
    override def shortStatus = "--"
  }
  case class Fail(testFile: File, reason: String, transcript: List[String]) extends TestState {
    def what = "fail"
  }
  case class Crash(testFile: File, caught: Throwable, transcript: List[String]) extends TestState {
    def what = "crash"
    def reason = s"caught $caught_s - ${caught.getMessage}"

    private def caught_s = (caught.getClass.getName split '.').last
    private def stack_s  = stackTraceString(caught)
    override def transcriptString = nljoin(super.transcriptString, caught_s)
  }
}
