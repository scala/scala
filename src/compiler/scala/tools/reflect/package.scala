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

package scala.tools

import scala.language.implicitConversions
import scala.reflect.api.JavaUniverse
import scala.reflect.internal.Reporter
import scala.reflect.internal.util.{CodeAction, Position}
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.{ConsoleReporter, FilteringReporter}

package object reflect {
  // [todo: can we generalize this?
  import scala.reflect.runtime.{universe => ru}
  implicit def ToolBox(mirror0: ru.Mirror): ToolBoxFactory[ru.type] =
    new ToolBoxFactory[ru.type](mirror0.universe) {
      lazy val mirror = mirror0
    }

  // todo. replace this with an implicit class, once the pesky warning is gone
  // we don't provide `Eval` for trees, because it's unclear where to get an evaluation mirror from
  implicit def Eval[T](expr: JavaUniverse # Expr[T]): Eval[T] = new Eval[T](expr)

  /** Creates a UI-less reporter that simply accumulates all the messages
   */
  def mkSilentFrontEnd(): FrontEnd = new FrontEnd {
    def display(info: Info): Unit = ()
  }

  /** Creates a reporter that prints messages to the console according to the settings.
   *
   *  `minSeverity` determines minimum severity of the messages to be printed.
   *  0 stands for INFO, 1 stands for WARNING and 2 stands for ERROR.
   */
  // todo. untangle warningsAsErrors from Reporters. I don't feel like moving this flag here!
  def mkConsoleFrontEnd(minSeverity: Int = 1): FrontEnd = {
    val settings = new Settings()
    if (minSeverity <= 0) settings.verbose.value = true
    if (minSeverity > 1) settings.nowarn.value = true
    reporterToFrontEnd(new ConsoleReporter(settings))
  }

  private[reflect] def reporterToFrontEnd(reporter: FilteringReporter): FrontEnd = new FrontEnd {
    val API_INFO = INFO
    val API_WARNING = WARNING
    val API_ERROR = ERROR

    override def hasErrors   = reporter.hasErrors
    override def hasWarnings = reporter.hasWarnings

    def display(info: Info): Unit = info.severity match {
      case API_INFO    => reporter.echo(info.pos, info.msg)
      case API_WARNING => reporter.warning(info.pos, info.msg)
      case API_ERROR   => reporter.error(info.pos, info.msg)
      case x           => throw new MatchError(x)
    }

    override def flush(): Unit = {
      super.flush()
      reporter.flush()
    }

    override def reset(): Unit = {
      super.reset()
      reporter.reset()
    }
  }

  private[reflect] def frontEndToReporter(frontEnd: FrontEnd, settings0: Settings): FilteringReporter = new FilteringReporter {
    val settings = settings0

    val API_INFO    = frontEnd.INFO
    val API_WARNING = frontEnd.WARNING
    val API_ERROR   = frontEnd.ERROR

    type NscSeverity = Reporter.Severity
    val NSC_INFO     = Reporter.INFO
    val NSC_WARNING  = Reporter.WARNING
    val NSC_ERROR    = Reporter.ERROR

    override def doReport(pos: Position, msg: String, nscSeverity: NscSeverity, actions: List[CodeAction]): Unit =
      frontEnd.log(pos, msg, (nscSeverity: @unchecked) match {
        case NSC_INFO => API_INFO
        case NSC_WARNING => API_WARNING
        case NSC_ERROR => API_ERROR
      })

    override def flush(): Unit = {
      super.flush()
      frontEnd.flush()
    }

    override def reset(): Unit = {
      super.reset()
      frontEnd.reset()
    }
  }
}

package reflect {
  class Eval[T](expr: JavaUniverse # Expr[T]) {
    def eval: T = {
      val factory = new ToolBoxFactory[JavaUniverse](expr.mirror.universe) { val mirror = expr.mirror.asInstanceOf[this.u.Mirror] }
      val toolBox = factory.mkToolBox()
      toolBox.eval(expr.tree.asInstanceOf[toolBox.u.Tree]).asInstanceOf[T]
    }
  }
}
