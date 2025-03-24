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

package scala.tools.partest.nest

import scala.tools.nsc.Properties.envOrElse
import scala.util.chaining._
import Spec.Info

/** Machinery for what amounts to a command line specification DSL.
 *  It is designed so the same specification trait can be used for
 *  two different purposes: generating a singleton specification object
 *  (trait Reference) and providing well typed vals for every configurable
 *  option in response to any given set of arguments (trait Instance).
 */
object Opt {
  trait Error {
    self: Implicit =>

    protected def fail(msg: String) = runAndExit(println(programInfo.runner + ": " + msg))
    protected def failOption(arg: String, why: String) = fail(s"$opt: '$arg' is $why")
  }

  trait Implicit {
    def name: String
    def programInfo: Info
    protected def opt = fromOpt(name)

    def --? : Boolean                       // --opt is set
    def --> (body: => Unit): Boolean        // if --opt is set, execute body
    def --| : Option[String]                // --opt <arg: String> is optional, result is Option[String]
    def --^[T: FromString] : Option[T]      // --opt <arg: T> is optional, result is Option[T]

    def optMap[T](f: String => T) = --| map f

    /** Names.
     */
    def defaultTo[T: FromString](default: T): T
    def defaultToEnv(envVar: String): String
    def choiceOf[T: FromString](choices: T*): Option[T]
    def expandTo(args: String*): Unit

    /** Help.
     */
    def /(descr: String): String            // --opt has help description 'descr'
  }

  class Reference(val programInfo: Info, val options: Reference.Accumulators, val name: String) extends Implicit {
    import options._

    def --?                             = { addUnary(opt) ; false }
    def --> (body: => Unit)             = { addUnary(opt) ; false }
    def --|                             = { addBinary(opt) ; None }
    def --^[T: FromString]              = { addBinary(opt) ; None }

    def defaultTo[T: FromString](default: T)  = { addBinary(opt) ; addHelpDefault(() => default.toString) ; default }
    def defaultToEnv(envVar: String)          = { addBinary(opt) ; addHelpEnvDefault(envVar) ; "" }
    def choiceOf[T: FromString](choices: T*)  = { addBinary(opt) ; None }
    def expandTo(args: String*)               = { addExpand(name, args.toList) ; addHelpAlias(() => args mkString " ") }

    def /(descr: String)                = name.tap(_ => addHelp(() => helpFormatStr.format(opt, descr)))
  }

  class Instance(val programInfo: Info, val parsed: CommandLine, val name: String) extends Implicit with Error {
    def --?                             = parsed isSet opt
    def --> (body: => Unit)             = { val isSet = parsed isSet opt ; if (isSet) body ; isSet }
    def --|                             = parsed get opt
    def --^[T: FromString]              = {
      val fs = implicitly[FromString[T]]
      --|.map { arg =>
        if (fs isDefinedAt arg) fs(arg)
        else failOption(arg, "not a " + fs.targetString)
      }
    }

    def defaultTo[T: FromString](default: T)  = --^[T] getOrElse default
    def defaultToEnv(envVar: String)          = --| getOrElse envOrElse(envVar, "")
    def expandTo(args: String*)               = ()

    def choiceOf[T: FromString](choices: T*) = {
      --^[T] map { arg =>
        if (choices contains arg) arg
        else failOption(arg.toString, "not a valid choice from " + choices)
      }
    }

    def /(descr: String)                = name
  }
}
