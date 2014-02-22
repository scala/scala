package p1

private abstract class ProjectDef(val autoPlugins: Any) extends ProjectDefinition
sealed trait ResolvedProject extends ProjectDefinition {
  def autoPlugins: Any
}

sealed trait ProjectDefinition {
  private[p1] def autoPlugins: Any
}


object Test {
  // was "error: value autoPlugins in class ProjectDef of type Any cannot override final member"
  new ProjectDef(null) with ResolvedProject
}
