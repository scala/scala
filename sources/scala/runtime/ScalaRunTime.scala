package scala.runtime;

object ScalaRunTime {

    class Try[a](r: scala.runtime.ResultOrException[a]) {
      def Catch[b >: a](handler: PartialFunction[Throwable, b]): b =
	if (r.exc == null)
	    r.result.asInstanceOf[b]
	else if (/*!(r.exc is NonLocalReturn) && */handler isDefinedAt r.exc)
	    handler(r.exc)
	else
	    throw r.exc;

      def Finally(handler: Unit): a =
	if (r.exc == null) r.result.asInstanceOf[a] else throw r.exc;
    }

    def Try[a](def/*!!!*/ block: a): Try[a] =
        new Try(ResultOrException.tryBlock(block));

    def While(def/*!!!*/ cond: Boolean)(def/*!!!*/ body: Unit): Unit =
      NativeLoop.loopWhile(cond, body);

    trait DoWhile {
      def While(def/*!!!*/ condition: Boolean): Unit;
    }

    def Do(def/*!!!*/ command: Unit): DoWhile =
      new DoWhile {
	def While(def/*!!!*/ condition: Boolean): Unit = {
	  command;
          NativeLoop.loopWhile(condition, command);
        }
      }
}
