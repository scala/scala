import scala.tools.partest._
import java.io._
import scala.tools.nsc._
import scala.tools.nsc.util.CommandLineParser
import scala.tools.nsc.{Global, Settings, CompilerCommand}
import scala.tools.nsc.reporters.ConsoleReporter

import scala.tools.partest.trace
import scala.util.control.Exception._


object Test extends App {
  def intercept = allCatch.withApply(_.getClass)
  val t: Boolean = true
  trace(if (t) -0d else 0d)
  trace(if (t) 0d else -0d)
  trace(intercept(if (???) -0d else 0d))
  trace(intercept(if (???) 0d else 0d))
  trace(intercept(if (???) () else ()))
}
