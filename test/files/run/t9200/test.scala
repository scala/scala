trait W

trait T1
trait T2 extends T1

object O1 {
    type t = T1 with T2
}

class C1[w<:W](o: O1.t)

class C2 extends T1 with T2
