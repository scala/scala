object Test {
  M.cto // error
  M.m(M.cto, ()) // error
  M.m((), M.cto) // okay
  M.cto // error
}
