// scalac: -opt-warnings -opt:l:inline -opt-inline-from:** -Xfatal-warnings
class C {
    val cv = Map[Int, Int](1 -> 2)
    lazy val cl = Map[Int, Int](1 -> 2)
    def cd = Map[Int, Int](1 -> 2)
}
