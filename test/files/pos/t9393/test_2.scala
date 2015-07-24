class C {
  new bug.NamedImpl_1 // separate compilation, testing the classfile parser
  new bug.NamedImpl_2 // mixed compilation, testing the java source parser
}
