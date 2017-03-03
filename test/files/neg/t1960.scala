trait T { var vr: Int = 0 ; val vl: Int = 0 }
class C(vr: Int, vl: Int) extends T { def ref = vr + vl }
