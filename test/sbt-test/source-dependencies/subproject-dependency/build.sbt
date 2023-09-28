ThisBuild / incOptions := (ThisBuild / incOptions).value.withApiDebug(true)
lazy val dep = project
lazy val use = project.dependsOn(dep)
