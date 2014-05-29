object Test extends App {
  import reflect.ClassTag

  object SomeEnum extends Enumeration {
    val one, two, three, four = Value
  }

  def sctor[A <: Set[Int]](f: Int => A)(implicit A: ClassTag[A])
      : (String, Int => Set[Int]) =
    (A.runtimeClass.getSimpleName, f)

  val inits: Seq[(String, Int => Set[Int])] = {
    import collection.immutable.{Seq => _, _}
    Seq(sctor(BitSet(_)),
        sctor(HashSet(_)),
        sctor(ListSet(_)),
        sctor(SortedSet(_)),
        sctor(TreeSet(_)))
  }

  def sVarInfo[A](sa: Set[A]): String = {
    val saa = sa.toSet[Any]
    if (sa eq saa) s"""covariant (${(saa + "hi") contains "hi"})"""
    else "invariant"
  }

  inits foreach {case (name, singleton) =>
    print(s"${name}: ")
    val one = singleton(1)
    println(Seq(2,3,4).scanLeft(one)(_ + _) map sVarInfo toList)
  }

  println(s"ValueSet: ${sVarInfo(SomeEnum.values)}")
}
