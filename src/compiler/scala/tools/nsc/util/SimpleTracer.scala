package scala.tools.nsc
package util

// todo: We should unify this with Tracer. I'd do it but Tracer is
// too complicated for me to understand quickly.
import java.io.PrintStream

/** A simple tracer
 *  @param out: The print stream where trace info shoul be sent
 *  @param enabled: A condition that must be true for trace info to be produced.
 */
class SimpleTracer(out: PrintStream, enabled: Boolean = true) {
  def apply[T](msg: String)(value: T): T = {
    if (enabled) out.println(msg+value)
    value
  }
  def withOutput(out: PrintStream) = new SimpleTracer(out, enabled)
  def when(enabled: Boolean): SimpleTracer = new SimpleTracer(out, enabled)
}
