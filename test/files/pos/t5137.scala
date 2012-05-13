object Test {

  // okay
  (1 * (List[BigInt]().map(((x0) => x0 match {
    case x => x
  })).sum))

  // okay
  ((1: BigInt) * (List[BigInt]().map({
    case x => x
  }).sum))

  // fail
  (1 * (List[BigInt]().map({
    case x => x
  }).sum))
}