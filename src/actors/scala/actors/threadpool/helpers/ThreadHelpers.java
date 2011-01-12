/*
 * Written by Dawid Kurzyniec and released to the public domain, as explained
 * at http://creativecommons.org/licenses/publicdomain
 */
package scala.actors.threadpool.helpers;

/**
 * Emulation of some new functionality present in java.lang.Thread in J2SE 5.0.
 *
 * @author Dawid Kurzyniec
 * @version 1.0
 */
public class ThreadHelpers {

    private ThreadHelpers() {}

    /**
     * Returns wrapped runnable that ensures that if an exception occurs
     * during the execution, the specified exception handler is invoked.
     * @param runnable runnable for which exceptions are to be intercepted
     * @param handler the exception handler to call when exception occurs
     *        during execution of the given runnable
     * @return wrapped runnable
     */
    public static Runnable assignExceptionHandler(final Runnable runnable,
                                                  final UncaughtExceptionHandler handler)
    {
        if (runnable == null || handler == null) {
            throw new NullPointerException();
        }
        return new Runnable() {
            public void run() {
                try {
                    runnable.run();
                }
                catch (Throwable error) {
                    try {
                        handler.uncaughtException(Thread.currentThread(), error);
                    }
                    catch (Throwable ignore) {}
                }
            }
        };
    }

    /**
     * Abstraction of the exception handler which receives notifications of
     * exceptions occurred possibly in various parts of the sys. Exception
     * handlers present attractive approach to exception handling in multi-threaded
     * systems, as they can handle exceptions that occurred in different threads.
     * <p>
     * This class is analogous to Thread.UncaughtExceptionHandler in J2SE 5.0.
     * Obviously you cannot use it the same way, e.g. you cannot assign the
     * handler to the thread so that it is invoked when thread terminates.
     * However, it can be {@link ThreadHelpers#assignExceptionHandler emulated}.
     */
    public static interface UncaughtExceptionHandler {
        /**
         * Notification of the uncaught exception that occurred within specified
         * thread.
         * @param thread the thread where the exception occurred
         * @param error the exception
         */
        void uncaughtException(Thread thread, Throwable error);
    }
}
