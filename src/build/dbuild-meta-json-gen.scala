// Use this script to generate dbuild-meta.json

// To generate the file:
//  - check out https://github.com/typesafehub/dbuild
//  - run `sbt metadata/console`
//  - paste the code below

// The `version` field is required for the ProjMeta data structure. However, dbuild will
// overwrite the version specified here with the version number found in the build.number
// file, so the actual value doesn't matter, see ScalaBuildSystem:
// https://github.com/typesafehub/dbuild/blob/25b087759cc52876712c594ea4172148beea1310/support/src/main/scala/com/typesafe/dbuild/support/scala/ScalaBuildSystem.scala#L351

import com.typesafe.dbuild.model._

val meta =
  ProjMeta(version = "2.12.0", projects = Seq(
    Project("scala-library", "org.scala-lang",
      Seq(ProjectRef("scala-library", "org.scala-lang")),
      Seq.empty),
    Project("scala-reflect", "org.scala-lang",
      Seq(ProjectRef("scala-reflect", "org.scala-lang")),
      Seq(ProjectRef("scala-library", "org.scala-lang"))),
    Project("scala-compiler", "org.scala-lang",
      Seq(ProjectRef("scala-compiler", "org.scala-lang")),
      Seq(ProjectRef("scala-reflect", "org.scala-lang"),
          ProjectRef("scala-xml", "org.scala-lang.modules")
         )),

    // Project("scala-repl", "org.scala-lang",
    //   Seq(ProjectRef("scala-repl", "org.scala-lang")),
    //   Seq(ProjectRef("scala-compiler", "org.scala-lang"))), // jline

    // Project("scala-interactive", "org.scala-lang",
    //   Seq(ProjectRef("scala-interactive", "org.scala-lang")),
    //   Seq(ProjectRef("scala-compiler", "org.scala-lang"), ProjectRef("scaladoc", "org.scala-lang"))),

    // Project("scaladoc", "org.scala-lang",
    //   Seq(ProjectRef("scaladoc", "org.scala-lang")),
    //   Seq(ProjectRef("scala-compiler", "org.scala-lang"),ProjectRef("scala-partest", "org.scala-lang"), ProjectRef("scala-xml", "org.scala-lang"))),

    Project("scalap", "org.scala-lang",
      Seq(ProjectRef("scalap", "org.scala-lang")),
      Seq(ProjectRef("scala-compiler", "org.scala-lang")))

  ))

println(Utils.writeValueFormatted(meta))
