trait Box[ K[A[x]] ]

object Box {
   // type constructor composition
   sealed trait ∙[A[_], B[_]] { type l[T] = A[B[T]] }

   // composes type constructors inside K
   type SplitBox[K[A[x]], B[x]] = Box[ ({ type l[A[x]] = K[ (A ∙ B)#l] })#l ]

   def split[ K[A[x]], B[x] ](base: Box[K]): SplitBox[K,B] = ???

   class Composed[B[_], K[A[x]] ] {
      val box: Box[K] = ???

      type Split[ A[x] ] = K[ (A ∙ B)#l ]
      val a: Box[Split] = Box.split(box)

      //Either of these work:
      //val a: Box[Split] = Box.split[K,B](box)
      //val a: Box[ ({ type l[A[x]] = K[ (A ∙ B)#l ] })#l ] = Box.split(box)
   }
}
