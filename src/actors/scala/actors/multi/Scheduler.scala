/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2005-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.actors.multi

import scala.collection.mutable._

/**
 * @author Philipp Haller
 */
abstract class IScheduler /*extends java.util.concurrent.Executor*/ {
  def execute(item: ReceiverTask): Unit
  def getTask(worker: WorkerThread): Runnable
  def tick(a: MailBox): Unit
  def getProcess(t: Thread): Process

  val QUIT_TASK = new Runnable() {
    def run(): Unit = {}
    override def toString() = "QUIT_TASK"
  }
}


object Scheduler /*extends java.util.concurrent.Executor*/ {
  private var sched: /*java.util.concurrent.Executor*/ IScheduler =
    //java.util.concurrent.Executors.newFixedThreadPool(2);
    //new FixedWorkersScheduler(2);
    new SpareWorkerScheduler2
    //new SpareWorkerScheduler

  def impl = sched
  def impl_= (scheduler: /*java.util.concurrent.Executor*/ IScheduler) = {
    Debug.info("Using scheduler " + scheduler)
    sched = scheduler
  }

  def execute(item: ReceiverTask) =
    sched.execute(item)

  def tick(a: MailBox) =
    sched.tick(a)

  def getProcess(t: Thread): Process =
    sched.getProcess(t)
}


class SpareWorkerScheduler2 extends IScheduler {
  private val tasks = new Queue[ReceiverTask];
  private var workers: Buffer[WorkerThread] = new ArrayBuffer[WorkerThread];

  val idle = new Queue[WorkerThread];
  val ticks = new HashMap[WorkerThread, long]
  val executing = new HashMap[MailBox, WorkerThread]
  val rexec = new HashMap[Thread, MailBox]

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

  def tick(a: MailBox): unit = synchronized {
    ticksCnt = ticksCnt + 1
    executing.get(a) match {
      case None => // thread outside of scheduler; error("No worker thread associated with actor " + a)
      case Some(wt) =>
        ticks.update(wt, System.currentTimeMillis)
    }
  }

  def getProcess(t: Thread): Process = synchronized {
    rexec.get(t) match {
      case None => null
      case Some(p: Process) => p
    }
  }

  def execute(item: ReceiverTask): unit = synchronized {
    if (idle.length > 0) {
      val wt = idle.dequeue
      executing.update(item.actor, wt)
      rexec.update(wt, item.actor)
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
        rexec.update(newWorker, item.actor)

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
      rexec.update(worker, item.actor)
      item
    }
    else {
      idle += worker
      null
    }
  }
}

/**
 * @author Philipp Haller
 */
abstract class SpareWorkerScheduler extends IScheduler {
  private var canQuit = false;
  private val tasks = new Queue[ReceiverTask];
  private val idle = new Queue[WorkerThread];

  private var workers: Buffer[WorkerThread] = new ArrayBuffer[WorkerThread];

  def init = {
    for (val i <- List.range(0, 2)) {
      val worker = new WorkerThread(this)
      workers += worker
      worker.start()
    }
  }
  init;

  var maxWorkers = 0;

  def execute(item: ReceiverTask): unit = synchronized {
    if (idle.length == 0) {
      tasks += item
      // create new worker
      val newWorker = new WorkerThread(this)
      workers += newWorker
      maxWorkers = workers.length
      newWorker.start()
      //newWorker.execute(item)
    }
    else {
      canQuit = true
      idle.dequeue.execute(item)
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

  def tick(a: MailBox): unit = {}

  def haltExcept(w: WorkerThread) = {
    for (val i <- List.range(0, workers.length))
      if (workers(i) != w) workers(i).halt
    QUIT_TASK
  }
}

/**
 * @author Philipp Haller
 */
abstract class FixedWorkersScheduler(workercnt: int) extends IScheduler {
  private var canQuit = false;
  private val tasks = new Queue[ReceiverTask];
  private val idle = new Queue[WorkerThread];

  //Console.println("Running with " + workercnt + " workers")
  private var workers = new Array[WorkerThread](workercnt);

  def init = {
    for (val i <- List.range(0, workers.length)) {
      workers(i) = new WorkerThread(this)
      workers(i).start()
    }
  }
  init;

  def execute(item: ReceiverTask): unit = synchronized {
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

  def tick(a: MailBox): unit = {}

  def haltExcept(w: WorkerThread) = {
    for (val i <- List.range(0, workers.length))
      if (workers(i) != w) workers(i).halt
    QUIT_TASK
  }
}
