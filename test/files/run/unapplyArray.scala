object Test {
  def main(args:Array[String]): Unit = {
    val z = Array(1,2,3,4)
    val zs: Seq[Int] = z
    val za: Any = z

/*
    Console.println("z  is arr[int]"+z.isInstanceOf[Array[int]])
    Console.println("zs is arr[int]"+zs.isInstanceOf[Array[int]])
    Console.println("za is arr[int]"+ za.isInstanceOf[Array[int]])

    Console.println("z  is seq[int]"+z.isInstanceOf[Seq[int]])
    Console.println("zs is seq[int]"+zs.isInstanceOf[Seq[int]])
    Console.println("za is seq[int]"+ za.isInstanceOf[Seq[int]])

    Console.println("z  is anyref"+z.isInstanceOf[AnyRef])

    Console.println("z   useq "+ Seq.unapplySeq(z))
    Console.println("zs  useq "+ Seq.unapplySeq(zs))
    Console.println("za  useq "+ Seq.unapplySeq(za))

    Console.println("z   aseq "+ Seq.unapplySeq(z))
    Console.println("zs  aseq "+ Seq.unapplySeq(zs))
    Console.println("za  aseq "+ Seq.unapplySeq(za))
*/
    val zl = zs match {
      case Seq(xs@_*) => xs.length
    }
    assert(zl == 4)
  }
}
