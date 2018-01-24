object Test {
  valueOf[1]

  type SOne = 1

  valueOf[SOne]

  val one : 1 = 1

  valueOf[one.type]

  type SOne1 = one.type

  valueOf[SOne1]
}
