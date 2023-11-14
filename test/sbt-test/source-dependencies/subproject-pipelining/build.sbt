ThisBuild / incOptions := (ThisBuild / incOptions).value.withPipelining(true)
lazy val dep = project
lazy val use = project.dependsOn(dep)
