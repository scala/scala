package ok1 {
  case class test private (foo: Map[String, List[Int]],
                           bar: List[Int],
                           baz: Map[String, List[String]]) {}

  case object test {
    def getInstance = apply(Map.empty, List.empty, Map.empty)

    def apply(foo: Map[String, List[Int]],
              bar: List[Int],
              baz: Map[String, List[String]]) = new test(foo, bar, baz)
  }
}

package ok2 {
  case class test private (foo: Map[String, List[Int]],
                           bar: List[Int],
                           baz: Map[String, List[String]]) {}

  case object test {
    def getInstance = apply(Map.empty)

    def apply(foo: Map[String, List[Int]] = Map.empty,
              bar: List[Int] = List.empty,
              baz: Map[String, List[String]] = Map.empty) =
      new test(foo, bar, baz)
  }
}

package notok {
  case class test private (foo: Map[String, List[Int]],
                             bar: List[Int],
                             baz: Map[String, List[String]]) {}

  case object test {
    def getInstance = apply()

    def apply(foo: Map[String, List[Int]] = Map.empty,
              bar: List[Int] = List.empty,
              baz: Map[String, List[String]] = Map.empty) =
      new test(foo, bar, baz)
  }
}
