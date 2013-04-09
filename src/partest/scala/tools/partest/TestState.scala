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
  def transcriptString = transcript.mkString("\n")

  def identAndReason = testIdent + reasonString
  def status         = s"$what - $identAndReason"
  def longStatus     = status + transcriptString
  def reasonString   = if (reason == "") "" else s"  [$reason]"

  override def toString = status
}

object TestState {
  case class Uninitialized(testFile: File) extends TestState {
    def what = "uninitialized"
    def reason = what
    def transcript = Nil
  }
  case class Pass(testFile: File) extends TestState {
    final override def isOk = true
    def what = "pass"
    def transcript: List[String] = Nil
    def reason = ""
  }
  case class Skip(testFile: File, reason: String) extends TestState {
    override def isOk = true
    final override def isSkipped = true
    def transcript: List[String] = Nil
    def what = "skip"
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
