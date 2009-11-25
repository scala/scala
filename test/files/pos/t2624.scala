object Test {
  List(1).map(identity(_))
  List(1).map(identity) // this didn't typecheck before the fix
}
