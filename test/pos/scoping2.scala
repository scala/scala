object That {
     trait A {
         type T <: I;
         trait I {}
     }
     trait B {
     	type T <: J;
     	trait J {}
     }
     type C = A with B {
         type T <: I with J;
     }
}

