lazy val std = project
lazy val lib = project.dependsOn(std)
lazy val app = project.dependsOn(lib)
