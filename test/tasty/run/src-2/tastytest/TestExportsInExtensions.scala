package tastytest


object TestExportsInExtensions extends Suite("TestExportsInExtensions") {
  test(assert(ExportsInExtensions.bar(3) == 3))
  test(assert(ExportsInExtensions.baz(17)(3) == 2))
  test(assert(ExportsInExtensions.bam(3) == 9))
  test(assert(ExportsInExtensions.::(10)(2) == 10 - 2))  // same as for implicit class C
}
