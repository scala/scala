class A extends B {
    private def getBar = List(1,2,3)
    lazy val bar: List[Int] = getBar
}
