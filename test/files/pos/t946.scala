object pmbugbounds {
  trait Bar
  class Foo[t <: Bar] {}
            
  (new Foo[Bar]) match {
    case _ : Foo[x] => null
  }
}
// t946.scala:6:
//     case _ : Foo[x] => null
//              ^
// Relating types pt0, pt, pattp, pattp+pt, pt+pattp, result {
//        pt0  pmbugbounds.Foo[pmbugbounds.Bar]
//         pt  pmbugbounds.Foo[pmbugbounds.Bar]
//      pattp  pmbugbounds.Foo[x]
//   pattp+pt  pmbugbounds.Foo[x]
//   pt+pattp  pmbugbounds.Foo[pmbugbounds.Bar]
//     result  pmbugbounds.Foo[pmbugbounds.Bar]
//
//        pt0 =:= pt             pt0 ~:= pattp          pt0 ~:= pattp+pt       pt0 =:= pt+pattp       pt0 =:= result
//         pt ~:= pattp           pt ~:= pattp+pt        pt =:= pt+pattp        pt =:= result
//      pattp =:= pattp+pt     pattp ~:= pt+pattp     pattp ~:= result
//   pattp+pt ~:= pt+pattp  pattp+pt ~:= result
//   pt+pattp =:= result
//
// }