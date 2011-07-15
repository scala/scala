// $Id$


package scala.util

/* TODO: better documentation of return-type modification.
 * (Especially what means "Illegal answer type modification: ... andThen ...")
 */

/**
 * Delimited continuations are a feature for modifying the usual control flow
 * of a program.  To use continuations, provide the option `-P:continuations:enable`
 * to the Scala compiler or REPL to activate the compiler plugin.
 *
 * Below is an example of using continuations to suspend execution while awaiting
 * user input. Similar facilities are used in so-called continuation-based web frameworks.
 *
 * {{{
 *   def go =
 *     reset {
 *       println("Welcome!")
 *       val first = ask("Please give me a number")
 *       val second = ask("Please enter another number")
 *       printf("The sum of your numbers is: %d\n", first + second)
 *     }
 * }}}
 *
 * The `reset` is provided by this package and delimits the extent of the transformation.
 * The `ask` is a function that will be defined below. Its effect is to issue a prompt
 * and then suspend execution awaiting user input. Once the user provides an input value,
 * execution of the suspended block resumes.
 *
 * {{{
 *   val sessions = new HashMap[UUID, Int=>Unit]
 *   def ask(prompt: String): Int @cps[Unit] =
 *     shift {
 *       k: (Int => Unit) => {
 *         val id = uuidGen
 *         printf("%s\nrespond with: submit(0x%x, ...)\n", prompt, id)
 *         sessions += id -> k
 *       }
 *     }
 * }}}
 *
 * The type of `ask` includes a `@cps` annotation which drives the transformation.
 * The type signature `Int @cps[Unit]` means that `ask` should be used in a context
 * requiring an `Int`, but actually it will suspend and return `Unit`.
 *
 * The computation leading up to the first `ask` is executed normally. The remainder
 * of the reset block is wrapped into a closure that is passed as the parameter `k` to
 * the `shift` function, which can then decide whether and how to execute the
 * continuation. In this example, the continuation is stored in a sessions map for
 * later execution. This continuation includes a second call to `ask`, which is treated
 * likewise once the execution resumes.
 *
 * <h2>CPS Annotation</h2>
 *
 * The aforementioned `@cps[A]` annotation is an alias for the more general `@cpsParam[B,C]`
 * where `B=C`. The type `A @cps[B,C]` describes a term which yields a value of type `A` within
 * an evaluation context producing a value of type `B`. After the CPS transformation, this return
 * type is modified to `C`.
 *
 * The `@cpsParam` annotations are introduced by `shift` blocks, and propagate via the return
 * types to the dynamically enclosing context. The propagation stops upon reaching a `reset`
 * block.
 */

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
    new ControlContext((f:A=>B,g:Exception=>B) => fun(f), null.asInstanceOf[A])
  }

  def reifyR[A,B,C](ctx: => ControlContext[A,B,C]): ControlContext[A,B,C] = {
    ctx
  }

}
