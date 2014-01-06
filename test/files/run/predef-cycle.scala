class Force {
	val t1 = new Thread {
    override def run() {
      scala.`package`
    }
  }
  val t2 = new Thread {
    override def run() {
      scala.Predef
    }
  }
  t1.start()
  t2.start()
  t1.join()
  t2.join()
}

object Test {
  def main(args: Array[String]) {
     new Force()
  }
}

/* Was deadlocking:
"Thread-2" prio=5 tid=7f9637268000 nid=0x119601000 in Object.wait() [119600000]
   java.lang.Thread.State: RUNNABLE
  at scala.Predef$.<init>(Predef.scala:90)
  at scala.Predef$.<clinit>(Predef.scala)
  at Force$$anon$2.run(predef-cycle.scala:10)

"Thread-1" prio=5 tid=7f9637267800 nid=0x1194fe000 in Object.wait() [1194fb000]
   java.lang.Thread.State: RUNNABLE
  at scala.collection.immutable.Set$Set4.$plus(Set.scala:127)
  at scala.collection.immutable.Set$Set4.$plus(Set.scala:121)
  at scala.collection.mutable.SetBuilder.$plus$eq(SetBuilder.scala:24)
  at scala.collection.mutable.SetBuilder.$plus$eq(SetBuilder.scala:22)
  at scala.collection.generic.Growable$$anonfun$$plus$plus$eq$1.apply(Growable.scala:48)
  at scala.collection.generic.Growable$$anonfun$$plus$plus$eq$1.apply(Growable.scala:48)
  at scala.collection.immutable.List.foreach(List.scala:318)
  at scala.collection.generic.Growable$class.$plus$plus$eq(Growable.scala:48)
  at scala.collection.mutable.SetBuilder.$plus$plus$eq(SetBuilder.scala:22)
  at scala.collection.TraversableLike$class.to(TraversableLike.scala:629)
  at scala.collection.AbstractTraversable.to(Traversable.scala:105)
  at scala.collection.TraversableOnce$class.toSet(TraversableOnce.scala:267)
  at scala.collection.AbstractTraversable.toSet(Traversable.scala:105)
  at scala.runtime.ScalaRunTime$.<init>(ScalaRunTime.scala:50)
  at scala.runtime.ScalaRunTime$.<clinit>(ScalaRunTime.scala)
  at scala.collection.mutable.HashTable$HashUtils$class.elemHashCode(HashTable.scala)
  at scala.collection.mutable.HashMap.elemHashCode(HashMap.scala:39)
  at scala.collection.mutable.HashTable$class.findOrAddEntry(HashTable.scala:161)
  at scala.collection.mutable.HashMap.findOrAddEntry(HashMap.scala:39)
  at scala.collection.mutable.HashMap.put(HashMap.scala:75)
  at scala.collection.mutable.HashMap.update(HashMap.scala:80)
  at scala.sys.SystemProperties$.addHelp(SystemProperties.scala:64)
  at scala.sys.SystemProperties$.bool(SystemProperties.scala:68)
  at scala.sys.SystemProperties$.noTraceSupression$lzycompute(SystemProperties.scala:80)
  - locked <7b8b0e228> (a scala.sys.SystemProperties$)
  at scala.sys.SystemProperties$.noTraceSupression(SystemProperties.scala:80)
  at scala.util.control.NoStackTrace$.<init>(NoStackTrace.scala:31)
  at scala.util.control.NoStackTrace$.<clinit>(NoStackTrace.scala)
  at scala.util.control.NoStackTrace$class.fillInStackTrace(NoStackTrace.scala:22)
  at scala.util.control.BreakControl.fillInStackTrace(Breaks.scala:93)
  at java.lang.Throwable.<init>(Throwable.java:181)
  at scala.util.control.BreakControl.<init>(Breaks.scala:93)
  at scala.util.control.Breaks.<init>(Breaks.scala:28)
  at scala.collection.Traversable$.<init>(Traversable.scala:96)
  at scala.collection.Traversable$.<clinit>(Traversable.scala)
  at scala.package$.<init>(package.scala:46)
  at scala.package$.<clinit>(package.scala)
  at Force$$anon$1.run(predef-cycle.scala:4)
  */