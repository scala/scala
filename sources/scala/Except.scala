package scala;

class Except[a](r: scala.runtime.ResultOrException[a]) {
  def except[b >: a](handler: PartialFunction[Throwable, b]): b =
    if (r.exc == null) r.result as b
    else if (handler isDefinedAt r.exc) handler(r.exc)
    else r.exc.throw;
  def finally(def handler: Unit): a =
    if (r.exc == null) r.result as a else { handler; r.exc.throw }
}
