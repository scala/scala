trait T {
  // should not compile, because it would result in a VerifyError
  @native def foo = ???
}
