// $Id$

package scala.util.continuations


class cpsParam[-B,+C] extends StaticAnnotation with TypeConstraint

private class cpsSym[B] extends Annotation // implementation detail

private class cpsSynth extends Annotation // implementation detail

private class cpsPlus extends StaticAnnotation with TypeConstraint // implementation detail
private class cpsMinus extends Annotation // implementation detail



@serializable final class ControlContext[+A,-B,+C](val fun: (A => B, Throwable => B) => C, val x: A) {

  /*
    final def map[A1](f: A => A1): ControlContext[A1,B,C] = {
      new ControlContext((k:(A1 => B)) => fun((x:A) => k(f(x))), null.asInstanceOf[A1])
    }

    final def flatMap[A1,B1<:B](f: (A => ControlContext[A1,B1,B])): ControlContext[A1,B1,C] = {
      new ControlContext((k:(A1 => B1)) => fun((x:A) => f(x).fun(k)))
    }
  */


  final def map[A1](f: A => A1): ControlContext[A1,B,C] = {
    if (fun eq null)
      new ControlContext(null, f(x))
    else
      new ControlContext({ (k: A1 => B, thr: Throwable => B) =>
        fun( { (x:A) =>
          var done = false
          try {
            val res = f(x)
            done = true
            k(res)
          } catch {
            case ex if !done =>
              thr(ex)
          }
        }, thr)
      }, null.asInstanceOf[A1])
  }


  // it would be nice if @inline would turn the trivial path into a tail call.
  // unfortunately it doesn't, so we do it ourselves in SelectiveCPSTransform

  /*@inline*/ final def flatMap[A1,B1,C1<:B](f: (A => ControlContext[A1,B1,C1])): ControlContext[A1,B1,C] = {
    if (fun eq null)
      f(x).asInstanceOf[ControlContext[A1,B1,C]]
    else
      new ControlContext({ (k: A1 => B1, thr: Throwable => B1) =>
        fun( { (x:A) =>
          var done = false
          try {
            val ctxR = f(x)
            done = true
            val res: C1 = ctxR.foreachFull(k, thr) // => B1
            res
          } catch {
            case ex if !done =>
              thr(ex).asInstanceOf[B] // => B NOTE: in general this is unsafe!
          }                           // However, the plugin will not generate offending code
        }, thr.asInstanceOf[Throwable=>B]) // => B
      }, null.asInstanceOf[A1])
  }

  final def foreach(f: A => B) = foreachFull(f, throw _)

  def foreachFull(f: A => B, g: Throwable => B): C = {
    if (fun eq null)
      f(x).asInstanceOf[C]
    else
      fun(f, g)
  }


  final def isTrivial = fun eq null
  final def getTrivialValue = x.asInstanceOf[A]

  // need filter or other functions?

  final def flatCat[A1>:A,B1<:B,C1>:C<:B1](pf: PartialFunction[Throwable, ControlContext[A1,B1,C1]]): ControlContext[A1,B1,C1] = {
    if (fun eq null)
      this
    else {
      val fun1 = (ret1: A1 => B1, thr1: Throwable => B1) => {
        val thr: Throwable => B1 = { t: Throwable =>
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
            case t1 if captureExceptions => thr1(t1) // => E2
          }
        }
        foreachFull(ret1, thr)// fun(ret1, thr)  // => B
      }
      new ControlContext(fun1, null.asInstanceOf[A1])
    }
  }

  // TODO: finally
}
