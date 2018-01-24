object Test {
  { val _ = 0; 0 }: 0 // compiles

  { 0; 0 }: 0 // compiles

  { val 0 = 0; 0 }: 0 // compiles

  { (); 0 }: 0 // compiles

  { val _ = (); 0 }: 0 // compiles

  { val () = (); 0 }: 0 // compiles

  ({ val _ = 0; 0 } + 0): 0 // compiles

  ({ val _ = (); 0 } + 0): 0 // compiles

  ({ val () = (); 0 } + 0): 0 // does not compile

  ({ (); 0 } + 0): 0 // does not compile

  ({ 0; 0 } + 0): 0 // does not compile

  ({ val 0 = 0; 0 } + 0): 0 // does not compile
}
