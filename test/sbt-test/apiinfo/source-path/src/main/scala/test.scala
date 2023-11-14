object Test {
  // When the refchecks compiler phase checks if Tuple2 has the
  // @deprecated annotation, it forces the info for the
  // `@deprecatedInheritance` annotation (the only annotation on
  // class Tuple2.
  //
  // Because we are compiling with `-sourcpath` that contains a
  // source file for `deprecatedInheritance`, this triggers a
  // `compileLate` of that file (which basically runs all previous
  // compiler phases on that file.)
  //
  // `compileLate` assumes that all of the phases are subclasses
  // of `GlobalPhase`, rather than just `Phase`. This triggers a
  // `ClassCastException` when it encounters SBT's custom
  // API phase.
  new Tuple2("", "")
}