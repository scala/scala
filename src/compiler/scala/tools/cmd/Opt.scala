/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools
package cmd

import nsc.Properties.envOrElse
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
    protected def failOption(arg: String, why: String) = fail("%s: '%s' is %s".format(opt, arg, why))
  }

  trait Implicit {
    def name: String
    def programInfo: Info
    protected def opt = toOpt(name)

    def --? : Boolean                       // --opt is set
    def --> (body: => Unit): Unit           // if --opt is set, execute body
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
    def --> (body: => Unit)             = { addUnary(opt) }
    def --|                             = { addBinary(opt) ; None }
    def --^[T: FromString]              = { addBinary(opt) ; None }

    def defaultTo[T: FromString](default: T)  = { addBinary(opt) ; addHelpDefault(() => default.toString) ; default }
    def defaultToEnv(envVar: String)          = { addBinary(opt) ; addHelpEnvDefault(envVar) ; "" }
    def choiceOf[T: FromString](choices: T*)  = { addBinary(opt) ; None }
    def expandTo(args: String*)               = { addExpand(name, args.toList) ; addHelpAlias(() => args mkString " ") }

    def /(descr: String)                = returning(name)(_ => addHelp(() => helpFormatStr.format(opt, descr)))
  }

  class Instance(val programInfo: Info, val parsed: CommandLine, val name: String) extends Implicit with Error {
    def --?                             = parsed isSet opt
    def --> (body: => Unit)             = if (parsed isSet opt) body
    def --|                             = parsed get opt
    def --^[T: FromString]              = {
      val fs = implicitly[FromString[T]]
      --| map { arg =>
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
