/*                     __                                               *\
**     ________ ___   / /  ___     Scala Parallel Testing               **
**    / __/ __// _ | / /  / _ |    (c) 2007-2008, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id: $

package scala.tools.partest

import java.awt.event.{ActionEvent, ActionListener}
import java.io.{File, FileOutputStream, PrintStream}
import javax.swing.Timer

import scala.actors.Actor
import scala.tools.nsc.Settings

import utils.PrintMgr._

/**
 * @author  Adriaan Moors, Thomas Hofer
 * @version 1.0
 */
class MasterActor(testDir: File, out: PrintStream) extends Actor {
  import scala.actors.Actor._

  private final val testPathLen = testDir.getPath.length

  private final val WIDTH = 56
  private final val TIMEOUT = 1360000

  private var counter = 0
  private var failed = 0

  private var conservative = false

  private val globalSettings = new Settings(x => ())

  //private var numOfActors = Math.min(4, Math.max(Integer.parseInt(System.getProperty("actors.maxPoolSize")),
  private val numOfActors = 4
  //println("Starting with " + numOfActors + " actors...")

  private var workers = (for (i <- 0 until numOfActors) yield (new WorkerActor(this, new Settings(x => ()), new ExtConsoleReporter(globalSettings)), i)).toList

  private var workingOn: List[(Int, Test)] = List()

  private var timers = (for (i <- 0 until numOfActors) yield createTimer(workers(i)._1)).toList

  private var testsToRun: List[Test] = List()

  private var failedTests: List[Test] = List()

  private def createTimer(worker: WorkerActor): Timer = {
    val action: ActionListener = new ActionListener {
      def actionPerformed(event: ActionEvent) {
        val workerID = workers.find((_)._1 == worker) match {
          case Some(x) => x
          case None => (null, -1)
        }
        val test = workingOn.find((_)._1 == workerID._2) match {
          case Some(x) => x._2
          case None => null
        }
        println("Actor " + workerID._1 + " failed, while testing " + test.file.getPath)
        failedTests.find(_ == test) match {
          case Some(x) => //...
          case None => testFailed(workerID, test)
        }

      }
    }

    new Timer(TIMEOUT, action)
  }

  private def testFailed(actor: (WorkerActor, Int), test: Test) = {
    failedTests = test :: failedTests
    var newWorker = new WorkerActor(this, new Settings(x => ()), new ExtConsoleReporter(globalSettings))
    timers(actor._2).stop
    timers = timers.take(actor._2 - 1) ::: List(createTimer(newWorker)) ::: timers.drop(actor._2)
    newWorker.start
    //println("Started actor " + newWorker)
    workers = (newWorker, actor._2) :: workers.remove(_ == actor)

    timers(actor._2).start
    newWorker ! (test, true, conservative)
  }

  private def hasNextTest = !testsToRun.isEmpty

  private def nextTest(): Test = {
    val test = testsToRun.head
    testsToRun = testsToRun.tail
    test
  }

  def act() {
    loop {
      react {
        case (test: Test) =>
          testsToRun = test :: testsToRun

        case ("start", conservative: Boolean) =>
          this.conservative = conservative
          workers foreach ((x) => {
              if (hasNextTest) {
                x._1.start
                val test = nextTest()
                // TODO Change here should be x._1 ! (test, false, conservative)
                x._1 ! (test, false, conservative)
                timers(x._2).start
                workingOn = (x._2, test) :: workingOn
              }
            })

        case (kind: String, succeeded: Boolean, file: File) =>
          val workerID = workers.find((_)._1 == sender) match {
            case Some(x) => x
            case None => (null, -1)
          }
          if (workerID._2 != -1) {
            workingOn = workingOn.remove((_)._1 == workerID._2)
            if (hasNextTest) {
              val test = nextTest()
              // TODO Change here should be x._1 ! (test, false, conservative)
              sender ! (test, false, conservative)
              timers(workerID._2).restart
              workingOn = (workerID._2, test) :: workingOn
            } else {
              sender ! false
              timers(workerID._2).stop
            }
          } else {
            //Houston, we got a problem...
          }
          counter += 1
          printOutline("testing: ")
          val name = file.getPath.substring(testPathLen)
          print("[...]" + name + List.toString(List.make(WIDTH - name.length, ' ')) + "[")
          if (succeeded) {
            printSuccess("  OK  ")
          } else {
            failed += 1
            printFailure("FAILED")
          }
          println("]")
          if (workingOn.isEmpty) {
            out.println(failed)
            out.println(counter - failed)
            out.close
            println
            exit
          }

        case msg =>
          println(msg)
      }
    }
  }

}
