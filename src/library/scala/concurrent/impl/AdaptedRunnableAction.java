package scala.concurrent.impl;

import scala.concurrent.forkjoin.ForkJoinTask;
import java.util.concurrent.RunnableFuture;

/**
 * Adaptor for Runnables without results
 */
final class AdaptedRunnableAction extends ForkJoinTask<Void>
                                          /*implements RunnableFuture<Void>*/ {
    final Runnable runnable;
    AdaptedRunnableAction(Runnable runnable) {
        if (runnable == null) throw new NullPointerException();
        this.runnable = runnable;
    }
    public final Void getRawResult() { return null; }
    public final void setRawResult(Void v) { }
    public final boolean exec() { runnable.run(); return true; }
    public final void run() { invoke(); }
    private static final long serialVersionUID = 5232453952276885070L;
}
