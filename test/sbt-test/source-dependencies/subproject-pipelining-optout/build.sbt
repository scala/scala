lazy val dep = project
lazy val use = project.dependsOn(dep).settings(incOptions := incOptions.value.withPipelining(true))
