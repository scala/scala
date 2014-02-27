// -----------------------------------------------------------------------------
//
//  Scalax - The Scala Community Library
//  Copyright (c) 2005-8 The Scalax Project. All rights reserved.
//
//  The primary distribution site is http://scalax.scalaforge.org/
//
//  This software is released under the terms of the Revised BSD License.
//  There is NO WARRANTY.  See the file LICENSE for the full text.
//
// -----------------------------------------------------------------------------

package scala.tools.scalap
package scalax
package rules

import scala.collection.mutable

trait MemoisableRules extends Rules {
  def memo[In <: Memoisable, Out, A, X](key: AnyRef)(toRule: => In => Result[Out, A, X]) = {
    lazy val rule = toRule
    from[In] { in => in.memo(key, rule(in)) }
  }

  override def ruleWithName[In, Out, A, X](name: String, f: In => rules.Result[Out, A, X]) = super.ruleWithName(name, (in: In) => in match {
      case s: Memoisable => s.memo(name, f(in))
      case _ => f(in)
    })
}

trait Memoisable {
  def memo[A](key: AnyRef, a: => A): A
}


object DefaultMemoisable {
  var debug = false
}

trait DefaultMemoisable extends Memoisable {
  protected val map = new mutable.HashMap[AnyRef, Any]

  def memo[A](key: AnyRef, a: => A) = {
    map.getOrElseUpdate(key, compute(key, a)).asInstanceOf[A]
  }

  protected def compute[A](key: AnyRef, a: => A): Any = a match {
    case success: Success[_, _] => onSuccess(key, success); success
    case other =>
      if(DefaultMemoisable.debug) println(key + " -> " + other)
      other
  }

  protected def onSuccess[S, T](key: AnyRef,  result: Success[S, T])  {
    val Success(out, t) = result
    if(DefaultMemoisable.debug) println(key + " -> " + t + " (" + out + ")")
  }
}



