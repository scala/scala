// Add genprod to the build; It should be moved from `src/build` to `project` now that the Ant build is gone
sources in Compile += ((baseDirectory).value.getParentFile / "src" / "build" / "genprod.scala")
