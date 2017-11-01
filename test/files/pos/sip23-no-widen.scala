object Test {
  final val a : 1 = 1
  final val fails : 2 = a + a //fail
  final val works : 2 = a + 1

  final val aok = 1
  final val also_works : 2 = aok + aok
}
