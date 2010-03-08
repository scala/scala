object Naturals {
  trait NAT {
    type a[s[_ <: NAT] <: NAT, z <: NAT] <: NAT
    type v = a[SUCC, ZERO]
  }
  final class ZERO extends NAT {
    type a[s[_ <: NAT] <: NAT, z <: NAT] = z
  }
  final class SUCC[n <: NAT] extends NAT {
    type a[s[_ <: NAT] <: NAT, z <: NAT] = s[n#a[s, z]]
  }
  type _0 = ZERO
  type _1 = SUCC[_0]
  type _2 = SUCC[_1]
  type _3 = SUCC[_2]
  type _4 = SUCC[_3]
  type _5 = SUCC[_4]
  type _6 = SUCC[_5]


  // crashes scala-2.8.0 beta1
  trait MUL[n <: NAT, m <: NAT] extends NAT {
    trait curry[n[_[_], _], s[_]] { type f[z <: NAT] = n[s, z] }
    type a[s[_ <: NAT] <: NAT, z <: NAT] = n#a[curry[m#a, s]#f, z]
  }

}