package scala.actors;

/**
 * @author Philipp Haller
 */
class WorkerThread(sched: IScheduler) extends Thread {
  private var task: Runnable = null
  private var running = true

  def halt = synchronized {
    running = false
    notify()
  }

  def execute(r: Runnable) = synchronized {
    //Debug.info("WORK: " + this + ": Executing task " + r)
    task = r
    notify()
  }

  override def run(): unit = synchronized {
    while (running) {
      if (task != null) {
        task.run()
        //Debug.info("WORK: " + this + " has finished.")
      }
      //Debug.info("WORK: " + this + ": Getting new task...")
      task = sched.getTask(this)
      //Debug.info("WORK (" + this + "): got task " + task)
      if (task == sched.QUIT_TASK) running = false
      else if (task == null) wait()
    }
  }
}
