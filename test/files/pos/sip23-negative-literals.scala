object Test {
  type ~~[A, B]
  type nonNeg = 2 ~~ 2

  type neg0 = -2
  type neg1 = -2 ~~ 2
  type neg2 = 2 ~~ -2
  type neg3 = -2 ~~ -2
}
