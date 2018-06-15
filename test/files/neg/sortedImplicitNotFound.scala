import collection.{mutable => m, immutable => i}

object WeekDay extends Enumeration { type WeekDay = Value; val Mon, Tue, Wed, Thu, Fri, Sat, Sun = Value }

object Test {
  val o = new Object

  val ms = m.SortedSet(1,2,3)
  ms.map(_ => "")
  ms.map(_ => o)
  ms.unsorted.map(_ => o)
  ms.flatMap(_ => List(""))
  ms.flatMap(_ => List(o))
  ms.unsorted.flatMap(_ => List(o))
  ms.zip(List(""))
  ms.zip(List(o))
  ms.unsorted.zip(List(o))
  ms.collect{case _ => ""}
  ms.collect{case _ => o}
  ms.unsorted.collect{case _ => o}

  val is = i.SortedSet(1,2,3)
  is.map(_ => "")
  is.map(_ => o)
  is.unsorted.map(_ => o)
  is.flatMap(_ => List(""))
  is.flatMap(_ => List(o))
  is.unsorted.flatMap(_ => List(o))
  is.zip(List(""))
  is.zip(List(o))
  is.unsorted.zip(List(o))
  is.collect{case _ => ""}
  is.collect{case _ => o}
  is.unsorted.collect{case _ => o}

  val mb = m.BitSet(1,2,3)
  mb.map(x => x) : m.BitSet
  mb.map(_ => "")
  mb.map(_ => o)
  mb.unsorted.map(_ => o)
  mb.flatMap(x => List(x)) : m.BitSet
  mb.flatMap(_ => List(""))
  mb.flatMap(_ => List(o))
  mb.unsorted.flatMap(_ => List(o))
  mb.zip(List(1)) : m.SortedSet[(Int, Int)]
  mb.zip(List(""))
  mb.zip(List(o))
  mb.unsorted.zip(List(o))
  mb.collect{case x => x} : m.BitSet
  mb.collect{case _ => ""}
  mb.collect{case _ => o}
  mb.unsorted.collect{case _ => o}

 val ib = i.BitSet(1,2,3)
  ib.map(x => x) : i.BitSet
  ib.map(_ => "")
  ib.map(_ => o)
  ib.unsorted.map(_ => o)
  ib.flatMap(x => List(x)) : i.BitSet
  ib.flatMap(_ => List(""))
  ib.flatMap(_ => List(o))
  ib.unsorted.flatMap(_ => List(o))
  ib.zip(List(1)) : i.SortedSet[(Int, Int)]
  ib.zip(List(""))
  ib.zip(List(o))
  ib.unsorted.zip(List(o))
  ib.collect{case x => x} : i.BitSet
  ib.collect{case _ => ""}
  ib.collect{case _ => o}
  ib.unsorted.collect{case _ => o}

  val es = WeekDay.values
  es.map(_ => "")
  es.map(_ => o)
  es.unsorted.map(_ => o)
  es.flatMap(_ => List(""))
  es.flatMap(_ => List(o))
  es.unsorted.flatMap(_ => List(o))
  es.zip(List(""))
  es.zip(List(o)) // ah well...: diverging implicit expansion for type Ordering[(WeekDay.Value, Object)]
  es.unsorted.zip(List(o))
  es.collect{case _ => ""}
  es.collect{case _ => o}
  es.unsorted.collect{case _ => o}

  val mm = m.SortedMap(1 -> o)
  mm.map(_ => ("", o))
  mm.map(_ => (o, o))
  mm.unsorted.map(_ => (o, o))
  mm.flatMap(_ => List(("", o)))
  mm.flatMap(_ => List((o, o)))
  mm.unsorted.flatMap(_ => List((o, o)))
  mm.collect{case _ => ("", o)}
  mm.collect{case _ => (o, o)}
  mm.unsorted.collect{case _ => (o, o)}

  val im = i.SortedMap(1 -> o)
  im.map(_ => ("", o))
  im.map(_ => (o, o))
  im.unsorted.map(_ => (o, o))
  im.flatMap(_ => List(("", o)))
  im.flatMap(_ => List((o, o)))
  im.unsorted.flatMap(_ => List((o, o)))
  im.collect{case _ => ("", o)}
  im.collect{case _ => (o, o)}
  im.unsorted.collect{case _ => (o, o)}
}
