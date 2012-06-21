/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2011 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck.util

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader
import scala.util.parsing.input.Position
import scala.collection.Set
import org.scalacheck.Test

trait CmdLineParser extends Parsers {

  type Elem = String

  trait Opt[+T] {
    val default: T
    val names: Set[String]
    val help: String
  }
  trait Flag extends Opt[Unit]
  trait IntOpt extends Opt[Int]
  trait FloatOpt extends Opt[Float]
  trait StrOpt extends Opt[String]

  class OptMap {
    private val opts = new collection.mutable.HashMap[Opt[_], Any]
    def apply(flag: Flag): Boolean = opts.contains(flag)
    def apply[T](opt: Opt[T]): T = opts.get(opt) match {
      case None => opt.default
      case Some(v) => v.asInstanceOf[T]
    }
    def update[T](opt: Opt[T], optVal: T) = opts.update(opt, optVal)
  }

  val opts: Set[Opt[_]]

  private class ArgsReader(args: Array[String], i: Int) extends Reader[String] {
    val pos = new Position {
      val column = (args take i).foldLeft(1)(_ + _.length + 1)
      val line = 1
      val lineContents = args.mkString(" ")
    }
    val atEnd = i >= args.length
    def first = if(atEnd) null else args(i)
    def rest = if(atEnd) this else new ArgsReader(args, i+1)
  }

  private def getOpt(s: String) = {
    if(s == null || s.length == 0 || s.charAt(0) != '-') None
    else opts.find(_.names.contains(s.drop(1)))
  }

  private val opt: Parser[Opt[Any]] = accept("option name", {
    case s if getOpt(s).isDefined => getOpt(s).get
  })

  private val strVal: Parser[String] = accept("string", {
    case s if s != null => s
  })

  private val intVal: Parser[Int] = accept("integer", {
    case s if s != null && s.length > 0 && s.forall(_.isDigit) => s.toInt
  })

  private val floatVal: Parser[Float] = accept("float", {
    case s if s != null && s.matches("[0987654321]+\\.?[0987654321]*")
      => s.toFloat
  })

  private case class OptVal[T](o: Opt[T], v: T)

  private val optVal: Parser[OptVal[Any]] = opt into {
    case o: Flag => success(OptVal(o, ()))
    case o: IntOpt => intVal ^^ (v => OptVal(o, v))
    case o: FloatOpt => floatVal ^^ (v => OptVal(o, v))
    case o: StrOpt => strVal ^^ (v => OptVal(o, v))
  }

  val options: Parser[OptMap] = rep(optVal) ^^ { xs =>
    val map = new OptMap
    xs.foreach { case OptVal(o,v) => map(o) = v }
    map
  }

  def printHelp = {
    println("Available options:")
    opts.foreach { opt =>
      println("  " + opt.names.map("-"+_).mkString(", ") + ": " + opt.help)
    }
  }

  def parseArgs[T](args: Array[String])(f: OptMap => T) =
    phrase(options map f)(new ArgsReader(args,0))
}
