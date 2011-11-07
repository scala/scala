package examples.pilib

/**
* From Pi to Scala: Semaphores, monitors, read/write locks.
* Readers/writers locks.
*/
object rwlock {

  import scala.concurrent.pilib._

  class Signal extends Chan[unit] {
    def send = write(())
    def receive = read
  }

  class CountLock {
    private val busy = new Signal
    def get = busy.send
    def release = busy.receive
    spawn < release >
  }

  /** A binary semaphore
  */
  class Lock {
    private val busy = new Signal;
    private val free = new Signal;
    def get = busy.send;
    def release = free.send;
    spawn < (while (true) {
      choice (
        busy * (x => free.receive),
        free * (x => ())
      )
    }) >
  }

  /** A monitor a la Java
  */
  class JavaMonitor {

    private val lock = new Lock

    private var waiting: List[Signal] = Nil

    def Wait = {
      val s = new Signal
      waiting = s :: waiting
      lock.release
      s.receive
      lock.get
    }

    def Notify =
      if (!waiting.isEmpty) {
        waiting.head.send
        waiting = waiting.tail
      }

    def NotifyAll =
      while (!waiting.isEmpty) {
        waiting.head.send
        waiting = waiting.tail
      }

    def await(cond: => boolean): unit =
      while (false == cond) (Wait)
  }

  /*
  class Buffer[a](size: Int) extends JavaMonitor with {
    var in = 0, out = 0, n = 0;
    val elems = new Array[a](size);
    def put(x: a) = synchronized {
      await(n < size);
      elems(out) = x;
      out = (out + 1) % size;
    }
    def get: a = synchronized {
      await(n > 0);
      val x = elems(in);
      in = (in + 1) % size;
      x
    }
  }
  */

  /** A readers/writers lock. */
  trait ReadWriteLock {
    def startRead: unit
    def startWrite: unit
    def endRead: unit
    def endWrite: unit
  }

  /**
  * A readers/writers lock, using monitor abstractions.
  */
  class ReadWriteLock1 extends JavaMonitor with ReadWriteLock {

    private var nactive: int = 0
    private var nwriters: int = 0

    def status =
      System.out.println(nactive + " active, " + nwriters + " writers");

    def startRead = synchronized {
      await(nwriters == 0)
      nactive = nactive + 1
      status
    }

    def startWrite = synchronized {
      nwriters = nwriters + 1
      await(nactive == 0)
      nactive = 1
      status
    }

    def endRead = synchronized {
      nactive = nactive - 1
      if (nactive == 0) NotifyAll
      status
    }

    def endWrite = synchronized {
      nwriters = nwriters - 1
      nactive = 0
      NotifyAll
      status
    }
  }

  /** A readers/writers lock, using semaphores
   */
  class ReadWriteLock2 extends ReadWriteLock {

    private var rc: int = 0  // reading readers
    private var wc: int = 0  // writing writers
    private var rwc: int = 0 // waiting readers
    private var wwc: int = 0 // waiting writers
    private val mutex = new Lock
    private val rsem = new Lock
    private val wsem = new Lock

    def startRead = {
      mutex.get;
      if (wwc > 0 || wc > 0) {
        rwc = rwc + 1;
        mutex.release;
        rsem.get;
        rwc = rwc - 1
      }
      rc = rc + 1;
      if (rwc > 0) rsem.release;
      mutex.release
    }

    def startWrite = {
      mutex.get;
      if (rc > 0 || wc > 0) {
        wwc = wwc + 1;
        mutex.release;
        wsem.get;
        wwc = wwc - 1
      }
      wc = wc + 1;
      mutex.release
    }

    def endRead = {
      mutex.get;
      rc = rc - 1;
      if (rc == 0 && wwc > 0) wsem.release;
      mutex.release
    }

    def endWrite = {
      mutex.get;
      wc = wc - 1;
      if (rwc > 0)
        rsem.release
      else if (wwc > 0) wsem.release;
      mutex.release
    }
  }

  /** A readers/writers lock, using channels, without priortities
  */
  class ReadWriteLock3 extends ReadWriteLock {

    private val sr = new Signal
    private val er = new Signal
    private val sw = new Signal
    private val ew = new Signal

    def startRead = sr.send
    def startWrite = sw.send
    def endRead = er.send
    def endWrite = ew.send

    private def rwlock: unit = choice (
      sr * (x => reading(1)),
      sw * (x => { ew.receive; rwlock })
    )

    private def reading(n: int): unit = choice (
      sr * (x => reading(n+1)),
      er * (x => if (n == 1) rwlock else reading(n-1))
    )

    spawn < rwlock >
  }

  /** Same, with sequencing
  */
  class ReadWriteLock4 extends ReadWriteLock {

    private val rwlock = new ReadWriteLock3

    private val sr = new Signal
    private val ww = new Signal
    private val sw = new Signal

    def startRead = sr.send
    def startWrite = { ww.send; sw.send }
    def endRead = rwlock.endRead
    def endWrite = rwlock.endWrite

    private def queue: unit = choice (
      sr * (x => { rwlock.startRead ; queue }),
      ww * (x => { rwlock.startWrite; sw.receive; queue })
    )

    spawn < queue >;
  }

  /** Readwritelock where writers always have priority over readers
  */
  class ReadWriteLock5 extends ReadWriteLock {

    private val sr = new Signal
    private val er = new Signal
    private val ww = new Signal
    private val sw = new Signal
    private val ew = new Signal

    def startRead = sr.send
    def startWrite = { ww.send; sw.send }
    def endRead = er.send
    def endWrite = ew.send

    private def Reading(nr: int, nw: int): unit = 
      if (nr == 0 && nw == 0)
        choice (
          sr * (x => Reading(1, 0)),
          ww * (x => Reading(0, 1))
        )
      else if (nr == 0 && nw != 0) {
        sw.receive;
        Writing(nw);
      }
      else if (nr != 0 && nw == 0)
        choice (
          sr * (x => Reading(nr + 1, 0)),
          er * (x => Reading(nr - 1, 0)),
          ww * (x => Reading(nr, 1))        
        )
      else if (nr != 0 && nw != 0)
        choice (
          ww * (x => Reading(nr, nw + 1)),
          er * (x => Reading(nr - 1, nw))
        );

    private def Writing(nw: int): unit  = choice (
      ew * (x => Reading(0, nw - 1)),
      ww * (x => Writing(nw + 1))
    );

    spawn < Reading(0, 0) >;

  }

  /**
   * Main function.
   */
  def main(args: Array[String]): unit = {
    val random = new java.util.Random()

    def reader(i: int, rwlock: ReadWriteLock): unit = {
      Thread.sleep(1 + random.nextInt(100))
      System.err.println("Reader " + i + " wants to read.")
      rwlock.startRead
      System.err.println("Reader " + i + " is reading.")
      Thread.sleep(1 + random.nextInt(100))
      rwlock.endRead
      System.err.println("Reader " + i + " has read.")
      reader(i, rwlock)
    }

    def writer(i: int, rwlock: ReadWriteLock): unit = {
      Thread.sleep(1 + random.nextInt(100))
      System.err.println("Writer " + i + " wants to write.")
      rwlock.startWrite
      System.err.println("Writer " + i + " is writing.")
      Thread.sleep(1 + random.nextInt(100))
      rwlock.endWrite
      System.err.println("Writer " + i + " has written.")
      writer(i, rwlock)
    }

    val n = try { Integer.parseInt(args(0)) } catch { case _ => 0 }
    if (n < 1 || 5 < n) {
      Console.println("Usage: scala examples.pilib.rwlock <n> (n=1..5)")
      exit
    }
    val rwlock = n match {
      case 1 => new ReadWriteLock1
      case 2 => new ReadWriteLock2
      case 3 => new ReadWriteLock3
      case 4 => new ReadWriteLock4
      case 5 => new ReadWriteLock5
    }
    List.range(0, 5) foreach (i => spawn < reader(i, rwlock) >)
    List.range(0, 5) foreach (i => spawn < writer(i, rwlock) >)
  }

}

