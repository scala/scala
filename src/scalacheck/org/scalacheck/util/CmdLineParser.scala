/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2018 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck.util

import scala.collection.Set

private[scalacheck] trait CmdLineParser {

  trait Opt[+T] {
    val default: T
    val names: Set[String]
    val help: String
  }
  trait Flag extends Opt[Unit]
  trait IntOpt extends Opt[Int]
  trait FloatOpt extends Opt[Float]
  trait StrOpt extends Opt[String]
  trait OpStrOpt extends Opt[Option[String]]

  class OptMap(private val opts: Map[Opt[_],Any] = Map.empty) {
    def apply(flag: Flag): Boolean = opts.contains(flag)
    def apply[T](opt: Opt[T]): T = opts.get(opt) match {
      case None => opt.default
      case Some(v) => v.asInstanceOf[T]
    }
    def set[T](o: (Opt[T], T)) = new OptMap(opts + o)
  }

  val opts: Set[Opt[_]]

  private def getOpt(s: String) = {
    if(s == null || s.length == 0 || s.charAt(0) != '-') None
    else opts.find(_.names.contains(s.drop(1)))
  }

  private def getStr(s: String) = Some(s)

  private def getInt(s: String) =
    if (s != null && s.length > 0 && s.forall(_.isDigit)) Some(s.toInt)
    else None

  private def getFloat(s: String) =
    if (s != null && s.matches("[0987654321]+\\.?[0987654321]*")) Some(s.toFloat)
    else None

  def printHelp(): Unit = {
    println("Available options:")
    opts.foreach { opt =>
      println("  " + opt.names.map("-"+_).mkString(", ") + ": " + opt.help)
    }
  }

  /** Parses a command line and returns a tuple of the parsed options,
   *  and any unrecognized strings */
  def parseArgs[T](args: Array[String]): (OptMap, List[String]) = {

    def parse(
      as: List[String], om: OptMap, us: List[String]
    ): (OptMap, List[String]) = as match {
      case Nil => (om, us)
      case a::Nil => getOpt(a) match {
        case Some(o: Flag) => parse(Nil, om.set((o,())), us)
        case _ => (om, us :+ a)
      }
      case a1::a2::as => (getOpt(a1) match {
        case Some(o: IntOpt) => getInt(a2).map(v => parse(as, om.set(o -> v), us))
        case Some(o: FloatOpt) => getFloat(a2).map(v => parse(as, om.set(o -> v), us))
        case Some(o: StrOpt) => getStr(a2).map(v => parse(as, om.set(o -> v), us))
        case Some(o: OpStrOpt) => getStr(a2).map(v => parse(as, om.set(o -> Option(v)), us))
        case _ => None
      }).getOrElse(parse(a2::as, om, us :+ a1))
    }

    parse(args.toList, new OptMap(), Nil)
  }
}
