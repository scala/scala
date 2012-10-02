/**
 * NOTE: Code snippets from this test are included in the Actor Migration Guide. In case you change
 * code in these tests prior to the 2.10.0 release please send the notification to @vjovanov.
 */
import scala.actors.Actor._
import scala.actors._
import scala.actors.migration._
import java.util.concurrent.{ TimeUnit, CountDownLatch }
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.concurrent.{ Promise, Await }

object Test {
  val finishedSingle, finishedSingle1, finishedLoop, finishedLoop1 = Promise[Boolean]

  def testDoubleReceive() = {
    println("Original")
    // Snippet that shows how to get rid of receive calls in Scala Actors.
    // This snippet is used in the Actors Migration Kit.
    val myActor = actor {
      println("do before")
      receive {
        case "hello" =>
          println("receive 1")
      }
      println("do in between")
      receive {
        case "hello" =>
          println("receive 1")
      }
      println("do after")
      finishedSingle.success(true)
    }

    myActor ! "hello"
    myActor ! "hello"

    Await.ready(finishedSingle.future, 5 seconds)
    println("Transformed")
    val myActorReact = actor {
      println("do before")
      react (({
        case "hello" =>
          println("receive 1")
      }: PartialFunction[Any, Unit]).andThen { x =>
        println("do in between")
        react (({
          case "hello" =>
            println("receive 1")
        }: PartialFunction[Any, Unit]).andThen { x =>
          println("do after")
          finishedSingle1.success(true)
        })
      })
    }

    myActorReact ! "hello"
    myActorReact ! "hello"

    Await.ready(finishedSingle1.future, 5 seconds)
  }

  def testLoopReceive() = {
    println("Test Loop Receive")
    // Snippet that shows how to get rid of receive calls in loops.
    // This snippet is used in the Actors Migration Kit.
    println("Original")
    val myActor = actor {
      var c = true
      while (c) {
        println("do before body")
        receive {
          case "hello" =>
            println("receive 1")
          case "exit" =>
            c = false
        }
        println("do after receive")
      }
      println("after loop")
      finishedLoop.success(true)
    }

    myActor ! "hello"
    myActor ! "exit"
    Await.ready(finishedLoop.future, 5 seconds)
    println("Transformed")

    val myActorReact = actor {
      var c = true
      loopWhile(c) {
        println("do before body")
        react (({
          case "hello" =>
            println("receive 1")
          case "exit" =>
            c = false
        }: PartialFunction[Any, Unit]).andThen { x =>
          println("do after receive")
          if (c == false) {
            println("after loop")
            finishedLoop1.success(true)
          }
        })
      }
    }

    myActorReact ! "hello"
    myActorReact ! "exit"

    Await.ready(finishedLoop1.future, 5 seconds)
  }

  def main(args: Array[String]) = {
    testDoubleReceive()
    testLoopReceive()
  }

}
