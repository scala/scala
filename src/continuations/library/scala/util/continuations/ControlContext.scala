// $Id$

package scala.util.continuations


class cpsParam[-B,+C] extends StaticAnnotation with TypeConstraint

private class cpsSym[B] extends Annotation // implementation detail

private class cpsSynth extends Annotation // implementation detail

private class cpsPlus extends StaticAnnotation with TypeConstraint // implementation detail
private class cpsMinus extends Annotation // implementation detail



@serializable final class ControlContext[+A,-B,+C](val fun: (A => B) => C, val x: A) {

  final def map[A1](f: (A => A1)): ControlContext[A1,B,C] = {
    if (fun eq null)
      new ControlContext(null, f(x))
    else
      new ControlContext((k:(A1 => B)) => fun((x:A) => k(f(x))), null.asInstanceOf[A1])
  }

  /*
    final def flatMap[A1,B1<:B](f: (A => ControlContext[A1,B1,B])): ControlContext[A1,B1,C] = {
      new ControlContext((k:(A1 => B1)) => fun((x:A) => f(x).fun(k)))
    }
  */

  // it would be nice if @inline would turn the trivial path into a tail call.
  // unfortunately it doesn't, so we do it ourselves in SelectiveCPSTransform

  /*@inline*/ final def flatMap[A1,B1,C1<:B](f: (A => ControlContext[A1,B1,C1])): ControlContext[A1,B1,C] = {
    if (fun eq null)
      f(x).asInstanceOf[ControlContext[A1,B1,C]]
    else
      new ControlContext({ k:(A1 => B1) =>
        fun { (x:A) =>
          val ctxR = f(x)
          val res: C1 = ctxR.foreach(k)
          res
        }
      }, null.asInstanceOf[A1])
  }

  final def foreach(f: (A => B)) = {
    if (fun eq null)
      f(x).asInstanceOf[C]
    else
      fun(f)
  }

  final def isTrivial = fun eq null
  final def getTrivialValue = x.asInstanceOf[A]

  // need filter or other functions?

}
