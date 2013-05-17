/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2010-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala.util.continuations

import scala.annotation.{ Annotation, StaticAnnotation, TypeConstraint }

/** This annotation is used to mark a parameter as part of a continuation
 * context.
 *
 * The type `A @cpsParam[B,C]` is desugared to `ControlContext[A,B,C]` at compile
 * time.
 *
 * @tparam B  The type of computation state after computation has executed, and
 *   before control is returned to the shift.
 * @tparam C  The eventual return type of this delimited compuation.
 * @see scala.util.continuations.ControlContext
 */
class cpsParam[-B,+C] extends StaticAnnotation with TypeConstraint

private class cpsSym[B] extends Annotation // implementation detail

private class cpsSynth extends Annotation // implementation detail

private class cpsPlus extends StaticAnnotation with TypeConstraint // implementation detail
private class cpsMinus extends Annotation // implementation detail


/**
 * This class represent a portion of computation that has a 'hole' in it.  The
 * class has the ability to compute state up until a certain point where the
 * state has the `A` type.  If this context is given a function of type
 * `A => B` to move the state to the `B` type, then the entire computation can
 * be completed resulting in a value of type `C`.
 *
 * An Example: {{{
 *   val cc = new ControlContext[String, String, String](
 *      fun = { (f: String=>String, err: Exception => String) =>
 *        val updatedState =
 *          try f("State")
 *          catch {
 *            case e: Exception => err(e)
 *          }
 *        updatedState + "-Complete!"
 *      },
 *      x = null.asIntanceOf[String]
 *  }
 *  cc.foreach(_ + "-Continued")  // Results in "State-Continued-Complete!"
 * }}}
 *
 * This class is used to transform calls to `shift` in the `continuations`
 * package.  Direct use and instantiation is possible, but usually reserved
 * for advanced cases.
 *
 *
 * A context may either be ''trivial'' or ''non-trivial''.   A ''trivial''
 * context '''just''' has a state of type `A`.  When completing the computation,
 * it's only necessary to use the function of type `A => B` directly against
 * the trivial value. A ''non-trivial'' value stores a computation '''around'''
 * the state transformation of type `A => B` and cannot be short-circuited.
 *
 * @param fun The captured computation so far.  The type
 *   `(A => B, Exception => B) => C` is a function where:
 *   - The first parameter `A=>B` represents the computation defined against
 *       the current state held in the ControlContext.
 *   - The second parameter `Exception => B` represents a computation to
 *       perform if an exception is thrown from the first parameter's computation.
 *   - The return value is the result of the entire computation contained in this
 *       `ControlContext`.
 * @param x  The current state stored in this context.  Allowed to be null if
 *   the context is non-trivial.
 * @tparam A  The type of the state currently held in the context.
 * @tparam B  The type of the transformed state needed to complete this computation.
 * @tparam C  The return type of the entire computation stored in this context.
 * @note `fun` and `x` are allowed to be `null`.
 * @see scala.util.continutations.shiftR
 */
final class ControlContext[+A,-B,+C](val fun: (A => B, Exception => B) => C, val x: A) extends Serializable {

  /*
    final def map[A1](f: A => A1): ControlContext[A1,B,C] = {
      new ControlContext((k:(A1 => B)) => fun((x:A) => k(f(x))), null.asInstanceOf[A1])
    }

    final def flatMap[A1,B1<:B](f: (A => ControlContext[A1,B1,B])): ControlContext[A1,B1,C] = {
      new ControlContext((k:(A1 => B1)) => fun((x:A) => f(x).fun(k)))
    }
  */

  /**
   * Modifies the currently captured state in this `ControlContext`.
   * @tparam A1 The new type of state in this context.
   * @param f A transformation function on the current state of the `ControlContext`.
   * @return The new `ControlContext`.
   */
  @noinline final def map[A1](f: A => A1): ControlContext[A1,B,C] = {
    if (fun eq null)
      try {
        new ControlContext[A1,B,C](null, f(x)) // TODO: only alloc if f(x) != x
      } catch {
        case ex: Exception =>
          new ControlContext((k: A1 => B, thr: Exception => B) => thr(ex).asInstanceOf[C], null.asInstanceOf[A1])
      }
    else
      new ControlContext({ (k: A1 => B, thr: Exception => B) =>
        fun( { (x:A) =>
          var done = false
          try {
            val res = f(x)
            done = true
            k(res)
          } catch {
            case ex: Exception if !done =>
              thr(ex)
          }
        }, thr)
      }, null.asInstanceOf[A1])
  }


  // it would be nice if @inline would turn the trivial path into a tail call.
  // unfortunately it doesn't, so we do it ourselves in SelectiveCPSTransform

  /**
   * Maps and flattens this `ControlContext` with another `ControlContext` generated from the current state.
   * @note   The resulting comuptation is still the type `C`.
   * @tparam A1 The new type of the contained state.
   * @tparam B1 The new type of the state after the stored continuation has executed.
   * @tparam C1 The result type of the nested `ControlContext`.  Because the nested `ControlContext` is executed within
   *   the outer `ControlContext`, this type must `>: B` so that the resulting nested computation can be fed through
   *   the current continuation.
   * @param f A transformation function from the current state to a nested `ControlContext`.
   * @return The transformed `ControlContext`.
   */
  @noinline final def flatMap[A1,B1,C1<:B](f: (A => ControlContext[A1,B1,C1])): ControlContext[A1,B1,C] = {
    if (fun eq null)
      try {
        f(x).asInstanceOf[ControlContext[A1,B1,C]]
      } catch {
        case ex: Exception =>
          new ControlContext((k: A1 => B1, thr: Exception => B1) => thr(ex).asInstanceOf[C], null.asInstanceOf[A1])
      }
    else
      new ControlContext({ (k: A1 => B1, thr: Exception => B1) =>
        fun( { (x:A) =>
          var done = false
          try {
            val ctxR = f(x)
            done = true
            val res: C1 = ctxR.foreachFull(k, thr) // => B1
            res
          } catch {
            case ex: Exception if !done =>
              thr(ex).asInstanceOf[B] // => B NOTE: in general this is unsafe!
          }                           // However, the plugin will not generate offending code
        }, thr.asInstanceOf[Exception=>B]) // => B
      }, null.asInstanceOf[A1])
  }

  /**
   * Runs the computation against the state stored in this `ControlContext`.
   * @param f the computation that modifies the current state of the context.
   * @note This method could throw exceptions from the computations.
   */
  final def foreach(f: A => B) = foreachFull(f, throw _)

  def foreachFull(f: A => B, g: Exception => B): C = {
    if (fun eq null)
      f(x).asInstanceOf[C]
    else
      fun(f, g)
  }

  /** @return true if this context only stores a state value and not any deferred computation. */
  final def isTrivial = fun eq null
  /** @return The current state value. */
  final def getTrivialValue = x.asInstanceOf[A]

  // need filter or other functions?

  final def flatMapCatch[A1>:A,B1<:B,C1>:C<:B1](pf: PartialFunction[Exception, ControlContext[A1,B1,C1]]): ControlContext[A1,B1,C1] = { // called by codegen from SelectiveCPSTransform
    if (fun eq null)
      this
    else {
      val fun1 = (ret1: A1 => B1, thr1: Exception => B1) => {
        val thr: Exception => B1 = { t: Exception =>
          var captureExceptions = true
          try {
            if (pf.isDefinedAt(t)) {
              val cc1 = pf(t)
              captureExceptions = false
              cc1.foreachFull(ret1, thr1) // Throw => B
            } else {
              captureExceptions = false
              thr1(t) // Throw => B1
            }
          } catch {
            case t1: Exception if captureExceptions => thr1(t1) // => E2
          }
        }
        fun(ret1, thr)// fun(ret1, thr)  // => B
      }
      new ControlContext(fun1, null.asInstanceOf[A1])
    }
  }

  final def mapFinally(f: () => Unit): ControlContext[A,B,C] = { // called in code generated by SelectiveCPSTransform
    if (fun eq null) {
      try {
        f()
        this
      } catch {
        case ex: Exception =>
          new ControlContext((k: A => B, thr: Exception => B) => thr(ex).asInstanceOf[C], null.asInstanceOf[A])
      }
    } else {
      val fun1 = (ret1: A => B, thr1: Exception => B) => {
        val ret: A => B = { x: A =>
          var captureExceptions = true
          try {
            f()
            captureExceptions = false
            ret1(x)
          } catch {
            case t1: Exception if captureExceptions => thr1(t1)
          }
        }
        val thr: Exception => B = { t: Exception =>
          var captureExceptions = true
          try {
            f()
            captureExceptions = false
            thr1(t)
          } catch {
            case t1: Exception if captureExceptions => thr1(t1)
          }
        }
        fun(ret, thr1)
      }
      new ControlContext(fun1, null.asInstanceOf[A])
    }
  }

}
