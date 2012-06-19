package scala.reflect.internal.util

import collection.mutable

object Statistics {

  /** If enabled, increment counter by one */
  @inline final def incCounter(c: Counter) {
    if (_enabled && c != null) c.value += 1
  }

  /** If enabled, increment counter by given delta */
  @inline final def incCounter(c: Counter, delta: Int) {
    if (_enabled && c != null) c.value += delta
  }

  /** If enabled, increment counter in map `ctrs` at index `key` by one */
  @inline final def incCounter[K](ctrs: QuantMap[K, Counter], key: K) =
    if (_enabled && ctrs != null) ctrs(key).value += 1

  /** If enabled, start subcounter. While active it will track all increments of
   *  its base counter.
   */
  @inline final def startCounter(sc: SubCounter): (Int, Int) =
    if (_enabled && sc != null) sc.start() else null

  /** If enabled, stop subcounter from tracking its base counter. */
  @inline final def stopCounter(sc: SubCounter, start: (Int, Int)) {
    if (_enabled && sc != null) sc.stop(start)
  }

  /** If enabled, start timer */
  @inline final def startTimer(tm: Timer): (Long, Long) =
    if (_enabled && tm != null) tm.start() else null

  /** If enabled, stop timer */
  @inline final def stopTimer(tm: Timer, start: (Long, Long)) {
    if (_enabled && tm != null) tm.stop(start)
  }

  /** If enabled, push and start a new timer in timer stack */
  @inline final def pushTimerClass(timers: ByClassTimerStack, cls: Class[_]): (Long, Long) =
    if (_enabled && timers != null) timers.push(cls) else null

  /** If enabled, stop and pop timer from timer stack */
  @inline final def popTimerClass(timers: ByClassTimerStack, prev: (Long, Long)) {
    if (_enabled && timers != null) timers.pop(prev)
  }

  /** Create a new counter that shows as `prefix` and is active in given phases */
  def newCounter(prefix: String, phases: String*) = new Counter(prefix, phases)

  /** Create a new relative counter that shows as `prefix` and is active
   *  in the same phases as its base counter. Relative counters print as percentages
   *  of their base counters.
   */
  def newRelCounter(prefix: String, ctr: Counter): Counter = new RelCounter(prefix, ctr)

  /** Create a new subcounter that shows as `prefix` and is active
   *  in the same phases as its base counter. Subcounters can track
   *  increments of their base counters and print as percentages
   *  of their base counters.
   */
  def newSubCounter(prefix: String, ctr: Counter): SubCounter = new SubCounter(prefix, ctr)

  /** Create a new counter that shows as `prefix` and is active in given phases */
  def newTimer(prefix: String, phases: String*): Timer = new Timer(prefix, phases)

  /** Create a new subtimer that shows as `prefix` and is active
   *  in the same phases as its base timer. Subtimers can track
   *  increments of their base timers and print as percentages
   *  of their base timers.
   */
  def newSubTimer(prefix: String, timer: Timer): Timer = new SubTimer(prefix, timer)

  /** Create a new view that shows as `prefix` and is active in given phases.
   *  The view always reflects the current value of `quant` as a quantity.
   */
  def newView(prefix: String, phases: String*)(quant: => Any): View = new View(prefix, phases,
quant)

  /** Create a new quantity map that shows as `prefix` and is active in given phases.
   */
  def newQuantMap[K, V <% Ordered[V]](prefix: String, phases: String*)(initValue: => V): QuantMap[K, V] = new QuantMap(prefix, phases, initValue)

  /** Same as newQuantMap, where the key type is fixed to be Class[_] */
  def newByClass[V <% Ordered[V]](prefix: String, phases: String*)(initValue: => V): QuantMap[Class[_], V] = new QuantMap(prefix, phases, initValue)

  /** Create a new timer stack map, indexed by Class[_]. */
  def newByClassTimerStack(prefix: String, underlying: Timer) = new ByClassTimerStack(prefix, underlying)

  def allQuantities: Iterable[Quantity] =
    for ((q, _) <- qs if !q.isInstanceOf[SubQuantity];
         r <- q :: q.children.toList if r.prefix.nonEmpty) yield r

  private def showPercent(x: Double, base: Double) =
    if (base == 0) "" else f" (${x / base * 100}%2.1f%)"

  trait Quantity {
    qs += (this -> ())
    val prefix: String
    val phases: Seq[String]
    def showAt(phase: String) = phases.isEmpty || (phases contains phase)
    def line = f"$prefix%-30s: ${this}"
    val children = new mutable.ListBuffer[Quantity]
  }

  trait SubQuantity extends Quantity {
    protected def underlying: Quantity
    underlying.children += this
  }

  class Counter(val prefix: String, val phases: Seq[String]) extends Quantity with Ordered[Counter] {
    var value: Int = 0
    def compare(that: Counter): Int =
      if (this.value < that.value) -1
      else if (this.value > that.value) 1
      else 0
    override def toString = value.toString
  }

  class View(val prefix: String, val phases: Seq[String], quant: => Any) extends Quantity {
    override def toString = quant.toString
  }

  private class RelCounter(prefix: String, val underlying: Counter) extends Counter(prefix, underlying.phases) with SubQuantity {
    override def toString =
      if (value == 0) "0"
      else {
        assert(underlying.value != 0, prefix+"/"+underlying.line)
        f"${value.toFloat / underlying.value}%2.1f"
      }
  }

  class SubCounter(prefix: String, override val underlying: Counter) extends Counter(prefix, underlying.phases) with SubQuantity {
    def start() = (value, underlying.value)
    def stop(prev: (Int, Int)) {
      val (value0, uvalue0) = prev
      value = value0 + underlying.value - uvalue0
    }
    override def toString =
      value + showPercent(value, underlying.value)
  }

  class Timer(val prefix: String, val phases: Seq[String]) extends Quantity with Ordered[Timer] {
    var nanos: Long = 0
    var timings = 0
    def compare(that: Timer): Int =
      if (this.nanos < that.nanos) -1
      else if (this.nanos > that.nanos) 1
      else 0
    def start() = {
      (nanos, System.nanoTime())
    }
    def stop(prev: (Long, Long)) {
      val (nanos0, start) = prev
      nanos = nanos0 + System.nanoTime() - start
      timings += 1
    }
    override def toString = s"$timings spans, ${nanos/1000}ms"
  }

  private class SubTimer(prefix: String, override val underlying: Timer) extends Timer(prefix, underlying.phases) with SubQuantity {
    override def toString: String = super.toString + showPercent(nanos, underlying.nanos)
  }

  /** A mutable map quantity where missing elements are automatically inserted
   *  on access by executing `initValue`.
   */
  class QuantMap[K, V <% Ordered[V]](val prefix: String, val phases: Seq[String], initValue: => V)
      extends scala.collection.mutable.HashMap[K, V] with Quantity {
    override def default(key: K) = {
      val elem = initValue
      this(key) = elem
      elem
    }
    override def toString =
      this.toSeq.sortWith(_._2 > _._2).map {
        case (cls: Class[_], elem) =>
          s"${cls.toString.substring(cls.toString.lastIndexOf("$") + 1)}: $elem"
        case (key, elem) =>
          s"$key: $elem"
      }.mkString(", ")
  }

  /** A mutable map quantity that takes class keys to subtimer values, relative to
   *  some `underlying` timer. In addition, class timers can be pushed and popped.
   *  Pushing the timer for a class means stopping the currently active timer.
   */
  class ByClassTimerStack(prefix: String, val underlying: Timer)
      extends QuantMap[Class[_], Timer](prefix, underlying.phases, new SubTimer("", underlying)) with SubQuantity {
    private var elems: List[(Timer, Long)] = Nil
    def push(cls: Class[_]): (Long, Long) = {
      val topTimer = this(cls)
      elems = (topTimer, 0L) :: elems
      topTimer.start()
    }
    def pop(prev: (Long, Long)) = {
      val (nanos0, start) = prev
      val duration = System.nanoTime() - start
      val (topTimer, nestedNanos) :: rest = elems
      topTimer.nanos = nanos0 + duration - nestedNanos
      topTimer.timings += 1
      elems = rest match {
        case (outerTimer, outerNested) :: elems1 =>
          (outerTimer, outerNested + duration) :: elems1
        case Nil =>
          Nil
      }
    }
  }

  private var _enabled = false
  private val qs = new mutable.WeakHashMap[Quantity, Unit]

  def enabled = _enabled
  def enabled_=(cond: Boolean) = {
    if (cond && !_enabled) {
      val test = new Timer("", Nil)
      val start = System.nanoTime()
      var total = 0L
      for (i <- 1 to 10000) {
        val time = System.nanoTime()
        total += System.nanoTime() - time
      }
      val total2 = System.nanoTime() - start
      println("Enabling statistics, measuring overhead = "+
              total/10000.0+"ns to "+total2/10000.0+"ns per timer")
      _enabled = true
    }
  }
}
