package scala.actors

import scala.collection.mutable._

object Scheduler /*extends java.util.concurrent.Executor*/ {
  private var sched: /*java.util.concurrent.Executor*/ IScheduler =
    //java.util.concurrent.Executors.newFixedThreadPool(4);
    //new FixedWorkersScheduler(2);
    //new SpareWorkerScheduler2;
    new SpareWorkerScheduler
    //new SingleThreadedScheduler

  def impl = sched
  def impl_= (scheduler: /*java.util.concurrent.Executor*/ IScheduler) = {
    sched = scheduler
  }

  def execute(task: Reaction) = synchronized {
    sched.execute(task)
  }

  def tick(a: Reactor) = {
    sched.tick(a)
  }

  def shutdown(): Unit = sched.shutdown()
}

abstract class IScheduler /*extends java.util.concurrent.Executor*/ {
  def execute(task: Reaction): Unit
  def getTask(worker: WorkerThread): Runnable
  def tick(a: Reactor): Unit

  def shutdown(): Unit

  val QUIT_TASK = new Reaction() {
    def actor: Reactor = null
    def run(): Unit = {}
    override def toString() = "QUIT_TASK"
  }
}

class SingleThreadedScheduler extends IScheduler {
  def execute(task: Reaction): Unit = {
    // execute task immediately on same thread
    task.run()
  }

  def getTask(worker: WorkerThread): Runnable = { null }

  def tick(a: Reactor): Unit = {}

  def shutdown(): Unit = {}
}

abstract class FixedWorkersScheduler(workercnt: int) extends IScheduler {
  private var canQuit = false;
  private val tasks = new Queue[ActorTask];
  private val idle = new Queue[WorkerThread];

  private var workers = new Array[WorkerThread](workercnt);

  def init = {
    for (val i <- List.range(0, workers.length)) {
      workers(i) = new WorkerThread(this)
      workers(i).start()
    }
  }
  init;

  def execute(item: ActorTask): unit = synchronized {
    if (workers.length == 0) item.run
    else {
      canQuit = true
      if (idle.length > 0) idle.dequeue.execute(item)
      else tasks += item
    }
  }

  def getTask(worker: WorkerThread) = synchronized {
    if (tasks.length > 0) tasks.dequeue
    else {
      idle += worker
      null
      //if ((idle.length == workers.length) && canQuit) haltExcept(worker)
      //else null
    }
  }

  def tick(a: Reactor): unit = {}
}

abstract class SpareWorkerScheduler2 extends IScheduler {
  private val tasks = new Queue[ActorTask];
  private var workers: Buffer[WorkerThread] = new ArrayBuffer[WorkerThread];

  val idle = new Queue[WorkerThread];
  val ticks = new HashMap[WorkerThread, long]
  val executing = new HashMap[Reactor, WorkerThread]

  var TICKFREQ = 50

  def init = {
    for (val i <- List.range(0, 2)) {
      val worker = new WorkerThread(this)
      workers += worker
      worker.start()
    }
  }
  init;

  var maxWorkers = 0;
  var ticksCnt = 0;

  def tick(a: Reactor): unit = synchronized {
    ticksCnt = ticksCnt + 1
    executing.get(a) match {
      case None => // thread outside of scheduler; error("No worker thread associated with actor " + a)
      case Some(wt) =>
        ticks.update(wt, System.currentTimeMillis)
    }
  }

  def execute(item: ActorTask): unit = synchronized {
    if (idle.length > 0) {
      val wt = idle.dequeue
      executing.update(item.actor, wt)
      wt.execute(item)
    }
    else {
      /* only create new worker thread
         when all are blocked according to heuristic

         we check time stamps of latest send/receive ops
         of ALL workers

         we stop if there is one which is not blocked */

      val iter = workers.elements
      var foundBusy = false
      while (iter.hasNext && !foundBusy) {
        val wt = iter.next
        ticks.get(wt) match {
          case None => foundBusy = true // assume not blocked
          case Some(ts) => {
            val currTime = System.currentTimeMillis
            if (currTime - ts < TICKFREQ)
              foundBusy = true
          }
        }
      }

      if (!foundBusy) {
        val newWorker = new WorkerThread(this)
        workers += newWorker
        maxWorkers = workers.length // statistics

        executing.update(item.actor, newWorker)

        newWorker.execute(item)
        newWorker.start()
      }
      else {
        // wait assuming busy thread will be finished soon
        // and ask for more work
        tasks += item
      }
    }
  }

  def getTask(worker: WorkerThread) = synchronized {
    if (tasks.length > 0) {
      val item = tasks.dequeue
      executing.update(item.actor, worker)
      item
    }
    else {
      idle += worker
      null
    }
  }
}

class SpareWorkerScheduler extends IScheduler {

  private val tasks = new Queue[Reaction]
  private val idle = new Queue[WorkerThread]
  private var workers: Buffer[WorkerThread] = new ArrayBuffer[WorkerThread]

  private var maxWorkers = 2

  def init() = {
    for (val i <- List.range(0, 2)) {
      val worker = new WorkerThread(this)
      workers += worker
      worker.start()
    }
  }
  init()

  def execute(task: Reaction): Unit = synchronized {
    if (!terminating) {
      if (idle.length == 0) {
        tasks += task
        // create new worker
        maxWorkers = maxWorkers + 1
        val newWorker = new WorkerThread(this)
        workers += newWorker
        newWorker.start()
      }
      else {
        idle.dequeue.execute(task)
      }
    }
  }

  def getTask(worker: WorkerThread) = synchronized {
    if (tasks.length > 0) tasks.dequeue
    else {
      idle += worker
      null
    }
  }

  def tick(a: Reactor): Unit = {}

  private var terminating = false

  def shutdown(): Unit = synchronized {
    terminating = true

    val numNonIdle = workers.length - idle.length

    for (val i <- List.range(0, numNonIdle))
      tasks += QUIT_TASK
    val idleThreads = idle.elements
    while (idleThreads.hasNext) {
      val worker = idleThreads.next
      worker.interrupt()
      worker.join()
    }
  }
}


class WorkerThread(sched: IScheduler) extends Thread {

  private var task: Runnable = null

  def execute(r: Runnable) = synchronized {
    task = r
    notify()
  }

  private var running = true

  override def run(): Unit = synchronized {
    try {
      while (running) {
        if (task != null) {
          task.run()
        }
        task = sched.getTask(this)
        if (task == sched.QUIT_TASK) {
          running = false
        } else if (task == null) wait()
      }
    } catch {
      case consumed: InterruptedException =>
        // allow thread to quit
    }
  }
}
