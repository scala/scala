/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2017 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck.util

import language.implicitConversions

import org.scalacheck.Prop.Arg
import org.scalacheck.Test

import math.round


sealed trait Pretty extends Serializable {
  def apply(prms: Pretty.Params): String

  def map(f: String => String) = Pretty(prms => f(Pretty.this(prms)))

  def flatMap(f: String => Pretty) = Pretty(prms => f(Pretty.this(prms))(prms))
}

object Pretty {

  case class Params(verbosity: Int)

  val defaultParams = Params(0)

  def apply(f: Params => String): Pretty = new Pretty { def apply(p: Params) = f(p) }

  def pretty[T](t: T, prms: Params)(implicit ev: T => Pretty): String = {
    val p = (t: Pretty) match {
      case null => prettyAny(null)
      case p => p
    }
    p(prms)
  }

  def pretty[T](t: T)(implicit ev: T => Pretty): String = pretty(t, defaultParams)

  private[this] implicit class StrBreak(val s1: String) extends AnyVal {
    def /(s2: String) = if(s2 == "") s1 else s1+"\n"+s2
  }

  def pad(s: String, c: Char, length: Int) =
    if(s.length >= length) s
    else s + List.fill(length-s.length)(c).mkString

  def break(s: String, lead: String, length: Int): String =
    if(s.length <= length || length <= 0) s
    else s.substring(0, length) / break(lead+s.substring(length), lead, length)

  def format(s: String, lead: String, trail: String, width: Int) =
    s.linesIterator.map(l => break(lead+l+trail, "  ", width)).mkString("\n")

  private def toStrOrNull(s: Any) = if (s == null) "null" else s.toString

  private[this] def escapeControlChars(s: String): String = {
    val builder = new StringBuilder
    @annotation.tailrec
    def loop(i: Int): Unit = {
      if(i < s.length){
        val c = s.codePointAt(i)
        if(Character.isISOControl(c)){
          builder.append("\\u%04x".format(c))
        }else{
          builder.append(s.charAt(i))
        }
        loop(i + 1)
      }
    }
    loop(0)
    builder.result()
  }

  implicit def prettyAny(t: Any) = Pretty { p => toStrOrNull(t) }

  implicit def prettyString(t: String) = Pretty { p => "\""++escapeControlChars(t)++"\"" }

  implicit def prettyList(l: List[Any]) = Pretty { p =>
    l.map("\""+_+"\"").mkString("List(", ", ", ")")
  }

  implicit def prettyThrowable(e: Throwable) = Pretty { prms =>
    val strs = e.getStackTrace.map { st =>
      import st._
      getClassName+"."+getMethodName + "("+getFileName+":"+getLineNumber+")"
    }

    val strs2 =
      if(prms.verbosity <= 0) Array[String]()
      else if(prms.verbosity <= 1) strs.take(5)
      else strs

    e.getClass.getName + ": " + e.getMessage / strs2.mkString("\n")
  }

  def prettyArgs(args: Seq[Arg[Any]]): Pretty = Pretty { prms =>
    if(args.isEmpty) "" else {
      for((a,i) <- args.zipWithIndex) yield {
        val l = "> "+(if(a.label == "") "ARG_"+i else a.label)
        val s =
          if(a.shrinks == 0) ""
          else "\n"+l+"_ORIGINAL: "+a.prettyOrigArg(prms)
        l+": "+a.prettyArg(prms)+""+s
      }
    }.mkString("\n")
  }

  implicit def prettyFreqMap(fm: FreqMap[Set[Any]]) = Pretty { prms =>
    if(fm.total == 0) ""
    else {
      "> Collected test data: " / {
        for {
          (xs,r) <- fm.getRatios
          ys = xs - (())
          if !ys.isEmpty
        } yield round(r*100)+"% " + ys.mkString(", ")
      }.mkString("\n")
    }
  }

  implicit def prettyTestRes(res: Test.Result) = Pretty { prms =>
    def labels(ls: collection.immutable.Set[String]) =
      if(ls.isEmpty) ""
      else "> Labels of failing property: " / ls.mkString("\n")
    val s = res.status match {
      case Test.Proved(args) => "OK, proved property."/prettyArgs(args)(prms)
      case Test.Passed => "OK, passed "+res.succeeded+" tests."
      case Test.Failed(args, l) =>
        "Falsified after "+res.succeeded+" passed tests."/labels(l)/prettyArgs(args)(prms)
      case Test.Exhausted =>
        "Gave up after only "+res.succeeded+" passed tests. " +
        res.discarded+" tests were discarded."
      case Test.PropException(args,e,l) =>
        "Exception raised on property evaluation."/labels(l)/prettyArgs(args)(prms)/
        "> Exception: "+pretty(e,prms)
    }
    val t = if(prms.verbosity <= 1) "" else "Elapsed time: "+prettyTime(res.time)
    s/t/pretty(res.freqMap,prms)
  }

  def prettyTime(millis: Long): String = {
    val min = millis/(60*1000)
    val sec = (millis-(60*1000*min)) / 1000d
    if(min <= 0) "%.3f sec ".format(sec)
    else "%d min %.3f sec ".format(min, sec)
  }

  implicit def prettyTestParams(prms: Test.Parameters) = Pretty { p =>
    prms.toString
  }
}
