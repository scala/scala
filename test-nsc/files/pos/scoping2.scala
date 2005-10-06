object That {
     trait A {
         type T <: I;
         trait I {}
     }
     trait B {
     	type T <: J;
     	trait J {}
     }
     trait C extends A with B {
         type T <: I with J;
     }
}

