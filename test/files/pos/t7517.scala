trait Box[ K[A[x]] ]

object Box {
   // type constructor composition
   sealed trait ∙[A[_], B[_]] { type l[T] = A[B[T]] }

   // composes type constructors inside K
   type SplitBox[K[A[x]], B[x]] = Box[ ({ type l[A[x]] = K[ (A ∙ B)#l] })#l ]

   def split[ K[A[x]], B[x] ](base: Box[K]): SplitBox[K,B] = ???

   class Composed[B[_], L[A[x]] ] {
      val box: Box[L] = ???

      type Split[ A[x] ] = L[ (A ∙ B)#l ]
      val a: Box[Split] = Box.split(box)

      //Either of these work:
      val a1: Box[Split] = Box.split[L,B](box)
      val a2: Box[ ({ type l[A[x]] = L[ (A ∙ B)#l ] })#l ] = Box.split(box)
   }
}