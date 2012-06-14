import sbt._
import Keys._
import ScalaBuildKeys._


object RemoteDependencies {
  def buildSettings(externalProjects: Set[URI], localScala: Setting[_]): Seq[Setting[_]] = Seq(
    commands += Command.command("fix-uri-projects") { (state: State) =>
      if(state.get(buildFixed) getOrElse false) state
      else {
        // TODO -fix up scalacheck's dependencies!
        val extracted = Project.extract(state)
        import extracted._
        val scalaVersionString = extracted get version

        def fix(s: Setting[_]): Setting[_] = s match {
          case ScopedExternalSetting(p, scalaInstance.key, setting) if externalProjects(p)        => localScala mapKey Project.mapScope(_ => s.key.scope)
          // TODO - Fix Actors dependency...
          //case ScopedExternalSetting(p, libraryDependencies.key, setting) if externalProjects(p)  => fixProjectDeps(s)
          case s                                                                                  => s
        }
        val transformed = session.mergeSettings map ( s => fix(s) )
        val scopes = transformed collect { case ScopedExternalSetting(p, _, s) if externalProjects(p) => s.key.scope } toSet
        // Create some fixers so we don't download scala or rely on it.
        // Also add dependencies that disappear in 2.10 for now...
        val fixers = for { scope <- scopes
                           setting <- Seq(autoScalaLibrary := false, 
                                          crossPaths := false,
                                          scalaVersion := scalaVersionString)
                     } yield setting mapKey Project.mapScope(_ => scope)
        val newStructure = Load.reapply(transformed ++ fixers, structure)
        Project.setProject(session, newStructure, state).put(buildFixed, true)
      }
    },
    onLoad in Global <<= (onLoad in Global) apply (_ andThen { (state: State) =>
      "fix-uri-projects" :: state
    })
  )
}



/** Matcher to make updated remote project references easier. */
object ScopedExternalSetting {
  def unapply[T](s: Setting[_]): Option[(URI, AttributeKey[_], Setting[_])] =
    s.key.scope.project match {
      case Select(p @ ProjectRef(uri, _)) => Some((uri, s.key.key, s))
      case _                              => None
    }
}



