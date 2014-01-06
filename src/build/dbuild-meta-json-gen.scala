// use this script to generate dbuild-meta.json
// make sure the version is specified correctly,
// update the dependency structure and
// check out distributed-build and run `sbt console`:
// TODO: also generate build.xml and eclipse config from a similar data-structure

import distributed.project.model._

val meta =
  ExtractedBuildMeta("2.11.0", Seq(
    Project("scala-library", "org.scala-lang",
      Seq(ProjectRef("scala-library", "org.scala-lang")),
      Seq.empty), // TODO: forkjoin
    Project("scala-reflect", "org.scala-lang",
      Seq(ProjectRef("scala-reflect", "org.scala-lang")),
      Seq(ProjectRef("scala-library", "org.scala-lang"))),
    Project("scala-compiler", "org.scala-lang",
      Seq(ProjectRef("scala-compiler", "org.scala-lang")),
      Seq(ProjectRef("scala-reflect", "org.scala-lang"),
          ProjectRef("scala-xml", "org.scala-lang.modules"),
          ProjectRef("scala-parser-combinators", "org.scala-lang.modules")
          // asm
         )),

    // Project("scala-repl", "org.scala-lang",
    //   Seq(ProjectRef("scala-repl", "org.scala-lang")),
    //   Seq(ProjectRef("scala-compiler", "org.scala-lang"))), // jline

    // Project("scala-interactive", "org.scala-lang",
    //   Seq(ProjectRef("scala-interactive", "org.scala-lang")),
    //   Seq(ProjectRef("scala-compiler", "org.scala-lang"), ProjectRef("scaladoc", "org.scala-lang"))),

    Project("scala-actors", "org.scala-lang",
      Seq(ProjectRef("scala-actors", "org.scala-lang")),
      Seq(ProjectRef("scala-library", "org.scala-lang"))),

    // Project("scaladoc", "org.scala-lang",
    //   Seq(ProjectRef("scaladoc", "org.scala-lang")),
    //   Seq(ProjectRef("scala-compiler", "org.scala-lang"),ProjectRef("scala-partest", "org.scala-lang"), ProjectRef("scala-xml", "org.scala-lang"), ProjectRef("scala-parser-combinators", "org.scala-lang"))),

    Project("scalap", "org.scala-lang",
      Seq(ProjectRef("scalap", "org.scala-lang")),
      Seq(ProjectRef("scala-compiler", "org.scala-lang")))

  ))

println(Utils.writeValue(meta))
