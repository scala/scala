// Add genprod to the build; It should be moved from `src/build` to `project` once the ANT build is gone
sources in Compile += ((baseDirectory).value.getParentFile / "src" / "build" / "genprod.scala")
