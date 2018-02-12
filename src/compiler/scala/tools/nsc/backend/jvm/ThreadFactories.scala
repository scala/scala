package scala.tools.nsc.backend.jvm

import java.util.concurrent.ThreadFactory
import java.util.concurrent.atomic.AtomicInteger

class CommonThreadFactory(namePrefix:String,
                              threadGroup: ThreadGroup = Thread.currentThread().getThreadGroup,
                              daemon:Boolean = true,
                              priority:Int = Thread.NORM_PRIORITY) extends ThreadFactory {
  private val group: ThreadGroup = Thread.currentThread().getThreadGroup
  private val threadNumber: AtomicInteger = new AtomicInteger(1)


  override def newThread(r: Runnable): Thread = {
    val t: Thread = new Thread(group, r, namePrefix + threadNumber.getAndIncrement, 0)
    if (t.isDaemon != daemon) t.setDaemon(daemon)
    if (t.getPriority != priority) t.setPriority(priority)
    t
  }
}
