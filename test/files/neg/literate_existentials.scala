
object LiterateExistentials {

//  Let's play with Scala's type system a bit.
//
//  From adriaanm, we have the following substitution rule, which allows us to
//  determine whether a type is a subtype of an existential in Scala:
//
//
//  T <: subst(U)    for all i: subst(Li) <: Vi /\ Vi <: subst(Hi)
//  --------------------------------------------------------------
//  T <: U forSome {type X1 :> L1 <: H1; ...; type Xn :> Ln <: Hn}
//
//  where subst(T) = T.subst(Xi, Vi) // Vi fresh type variables
//
//  T is a subtype of some existential if all constraints of the existential hold
//  after substituting Vi for the existentially quantified type variables Xi,
//  and T is a subtype of the underlying type U with the same substitution applied.
//
//
//  Since we are not a formal substitution system, we will actually be using
//  this rule 'backward' in order to determine whether it allows us to
//  truthfully make claims; In each example, we will start with the proposition
//  that a type is a subtype of an existential. Then, we will fit the
//  proposition into the form on the bottom rule by creating a set of bindings
//  which allow one to be transformed into the other. Next, we will express the
//  top of the substitution rule in terms of a series of constraints. We will
//  simplify those constraints until simple inspection can determine whether
//  they are consistent. From this, we can conclude whether the type system /
//  environment admit the top of the substitution rule (and thus, the bottom). If
//  they do, we can say that the proposition is true.


// In each case, we will also probe the compiler to see whether _it_ thinks that
// the proposition holds, using an uncommented implicitly[_ <:< _] line.




//  Proposition: Nothing :< (A forSome { type A >: String <: Any })
//
//
//  Bindings:
//  T  :=  Nothing
//  U  := A
//  X1 := A
//  L1 := String
//  H1 := Any
//
//  We need:
//
//  Nothing <: V1 // (U, which is "A", which V1 substituted for all instances of A)
//  String <: V1
//  V1 <: Any
//
//  Which simplify to:
//  V1 >: String <: Any 
//
//  That's not inconsistent, so we can say that:
//  T <: U forSome { type X1 >: L1 <: H1 }
//  which means (under our mappings):
//  Nothing <: A forSome { type A >: String <: Any }

// Now to ask the compiler:
  
  implicitly[Nothing <:< (A forSome { type A >: String <: Any })]


//  Let's try another:
//
//  Proposition: Int :< (M forSome { type M >: String <: Any })
//
//  Bindings:
//  T := Int
//  U := M
//  X1 := M
//  L1 := String
//  H1 := Any
//
//  We need:
//
//  Int <: V1
//  String <: V1
//  V1 <: Any
//
//  Which simplify to:
//
//  V1 >: lub(Int, String) <: Any 
//
//  V1 >: Any <: Any 
//
//  We have demonstrated consistency! We can say that:
//    T :< (U forSome { type U >: L1 <: H1 })
//  Under our bindings, this is:
//    Int :< (M forSome { type M >: String <: Any })
  
  implicitly[Int <:< (M forSome { type M >: String <: Any })]



//  Now, let's do a more complicated one:
//
//  Proposition: (Nothing, List[String]) <: ((A, B) forSome { type A >: String <: AnyRef; type B >: Null <: List[A] })
//
//  Bindings:
//  T  := (Nothing, List[String])
//  U  := (A, B)
//  X1 := A
//  X2 := B
//  L1 := String
//  H1 := AnyRef
//  L2 := Null
//  H2 := List[A]
//
//  We need:
//
//  (Nothing, List[String]) <: (V1, V2)
//  String <: V1
//  V1 <: AnyRef
//  Null <: V2
//  V2 <: List[V1]
//
//  Of course, we can split the first line to make:
//
//  Nothing <: V1
//  List[String]) <: V2
//  String <: V1
//  V1 <: AnyRef
//  Null <: V2
//  V2 <: List[V1]
//
//  Which reorder to:
//
//  Nothing <: V1
//  String <: V1
//  V1 <: AnyRef
//  List[String]) <: V2
//  Null <: V2
//  V2 <: List[V1]
//
//  Which simplify to:
//
//  String <: V1
//  V1 <: AnyRef
//  List[String]) <: V2
//  V2 <: List[V1]
//
//  String <: V1
//  V1 <: AnyRef
//  String <: V1
//
//  V1 >: String <: AnyRef
//
//  Consistency demonstrated! We can say that:
//  T <: U forSome {type X1 :> L1 <: H1; type X2 :> L2 <: H2}
//  meaning:
//  (Nothing, List[String]) <: ((A, B) forSome { type A >: String <: AnyRef; type B >: Null <: List[A] })

  implicitly[
    (Nothing, List[String]) <:< ((A, B) forSome { type A >: String <: AnyRef; type B >: Null <: List[A] })
   ]



//  Now let's try one that isn't true:
//
//  Proposition: Int :< (M forSome { type M >: Nothing <: String })
//
//  Bindings:
//  T  := Int
//  U  := M
//  X1 := M
//  L1 := Nothing
//  H1 := String
//
//  We need:
//
//  Int <: V1
//  Nothing <: V1
//  V1 <: String
//
//  V1 >: Int <: String 
//
//  Alas! These are inconsistent! There is no supertype of Int that is a
//  subtype of String! Our substitution rule does not allow us to claim that our
//  proposition is true.
//

  implicitly[Int <:< (M forSome { type M >: Nothing <: String })] // fails
// The preceding line causes the compiler to generate an error message.



//  Let's look at one final example, courtesy of paulp.
//  Proposition: String :< X forSome { type X >: Nothing <: String }
//
//  Bindings:
//  T  := String
//  U  := X
//  X1 := X
//  L1 := Nothing
//  H1 := String
//
//  We need:
//
//  String <: V1
//  Nothing <: V1
//  V1 <: String
//
//  Which simplify to:
//
//  String <: V1
//  V1 <: String
//
//  V1 >: String <: String
//
//  So, we can say:
//  T <: U forSome { type X1 >: L1 <: H1 }
//  which means:
//  String :< X forSome { type X >: Nothing <: String }

  implicitly[String <:< (X forSome { type X >: Nothing <: String })]

}
