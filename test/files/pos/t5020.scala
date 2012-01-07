package a {
  sealed trait GenericList[U, M[_ <: U]] {
    type Transformed[N[MMA <: U]] <: GenericList[U, N]
  }

  trait GenericCons[U, M[_ <: U], T <: GenericList[U, M]] extends GenericList[U, M] {
    type Transformed[N[MMB <: U]] = GenericCons[U, N, GenericList[U, M]#Transformed[N]]
  }
}

package b {
  sealed trait GenericList[L, M[_ >: L]] {
    type Transformed[N[MMA >: L]] <: GenericList[L, N]
  }

  trait GenericCons[L, M[_ >: L], T <: GenericList[L, M]] extends GenericList[L, M] {
    type Transformed[N[MMB >: L]] = GenericCons[L, N, T#Transformed[N]]
  }
}