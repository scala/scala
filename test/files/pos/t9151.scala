class t9151 {
  abstract class Foo extends Ordered[Foo]

  val seq: Seq[Int] = null
  val set: Set[Int] = null
  val map: Map[Int, Int] = null
  val values: Map[Int, Set[Foo]] = null

  map ++ set.map(_ -> "")

  values ++ seq.groupBy(_ / 2).toSeq.map({
    case (key, group) =>
      key -> (values(key) ++ group.map(_ => ""))
  })
}