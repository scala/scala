// $Id$


// TODO: scaladoc

package scala.util

package object continuations {

  type cps[A] = cpsParam[A,A]

  type suspendable = cps[Unit]


  def shift[A,B,C](fun: (A => B) => C): A @cpsParam[B,C] = {
    throw new NoSuchMethodException("this code has to be compiled with the Scala continuations plugin enabled")
  }

  def reset[A,C](ctx: =>(A @cpsParam[A,C])): C = {
    val ctxR = reify[A,A,C](ctx)
    if (ctxR.isTrivial)
      ctxR.getTrivialValue.asInstanceOf[C]
    else
      ctxR.foreach((x:A) => x)
  }

  def reset0[A](ctx: =>(A @cpsParam[A,A])): A = reset(ctx)

  def run[A](ctx: =>(Any @cpsParam[Unit,A])): A = {
    val ctxR = reify[Any,Unit,A](ctx)
    if (ctxR.isTrivial)
      ctxR.getTrivialValue.asInstanceOf[A]
    else
      ctxR.foreach((x:Any) => ())
  }


  // methods below are primarily implementation details and are not
  // needed frequently in client code

  def shiftUnit0[A,B](x: A): A @cpsParam[B,B] = {
    shiftUnit[A,B,B](x)
  }

  def shiftUnit[A,B,C>:B](x: A): A @cpsParam[B,C] = {
    throw new NoSuchMethodException("this code has to be compiled with the Scala continuations plugin enabled")
  }

  def reify[A,B,C](ctx: =>(A @cpsParam[B,C])): ControlContext[A,B,C] = {
    throw new NoSuchMethodException("this code has to be compiled with the Scala continuations plugin enabled")
  }

  def shiftUnitR[A,B](x: A): ControlContext[A,B,B] = {
    new ControlContext(null, x)
  }

  def shiftR[A,B,C](fun: (A => B) => C): ControlContext[A,B,C] = {
    new ControlContext(fun, null.asInstanceOf[A])
  }

  def reifyR[A,B,C](ctx: => ControlContext[A,B,C]): ControlContext[A,B,C] = {
    ctx
  }

}
