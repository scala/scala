package scalabuild

import java.io.FileWriter
import java.util.Properties

import sbt._
import Keys._

object JitWatchFilePlugin extends AutoPlugin {
  override def trigger = allRequirements
  override def requires = sbt.plugins.JvmPlugin
  val jitwatchConfigFileContents = taskKey[Properties]("Contents of file suitable for `jitwatch/launchUI -Djitwatch.config.file=jitwatch.properties`")
  val jitwatchConfigFile = taskKey[Unit]("file suitable for `jitwatch/launchUI.sh -Djitwatch.config.file=jitwatch.properties`")

  override lazy val projectSettings = List(Compile, Test).flatMap(c => inConfig(c)(jitwatchSettings))

  def jitwatchSettings: Seq[Setting[_]] = Seq(
    jitwatchConfigFileContents := {
      val sourcesValue = sources.value
      val depdependencyClasspathValue = dependencyClasspath.value ++ internalDependencyClasspath.value
      val props = new java.util.Properties
      val classpathString = (classDirectory.value +: depdependencyClasspathValue.map(_.data.toString)).mkString(",")
      val artifacts: Seq[Artifact] = depdependencyClasspathValue.flatMap(_.get(Keys.artifact.key))
      val dependencyModuleIds: Set[ModuleID] = depdependencyClasspathValue.flatMap(_.get(Keys.moduleID.key)).toSet
      props.put("Classes", classpathString)

      // JDK sources from $JAVA_HOME/src.zip
      val javaHomeSrc = {
        val javaDir = javaHome.value.getOrElse(new File(System.getProperty("java.home")))
        val src1 = javaDir / "src.zip"
        val src2 = javaDir.getParentFile / "src.zip"
        if (src1.exists()) src1 else src2
      }

      // Transitive sources from the projects that contribute to this classpath.
      val projects: Seq[ProjectRef] = buildDependencies.value.classpathTransitiveRefs(thisProjectRef.value) :+ thisProjectRef.value
      val projectArtifacts: Map[ProjectRef, Seq[Artifact]] = projects.map(project => (project -> (project / Keys.artifacts get settingsData.value).getOrElse(Nil))).toMap
      val artifactNameToProject: Map[String, Seq[ProjectRef]] = projects.groupBy(project => (project / Keys.name get settingsData.value).getOrElse(""))
      val transitiveSourceDirectories =  projects.flatMap { project =>
        val projectArtifacts: Seq[Artifact] = (project / Keys.artifacts get settingsData.value).getOrElse(Nil)
        val matching = projectArtifacts.filter(artifacts.contains(_))
        val configs = matching.flatMap(artifact => artifact.configurations).distinct
        val sourceDirectories: Seq[File] = configs.flatMap { configRef =>
          (project / sbt.Configuration.of(configRef.name.capitalize, configRef.name) / Keys.sourceDirectories).get(settingsData.value).toList.flatten
        }
        sourceDirectories
      }.distinct
      val transitiveSourceDirectories2 = artifacts.flatMap { artifact =>
        val projects = artifactNameToProject.getOrElse(artifact.name, Nil)
        projects.flatMap { project: ProjectRef =>
          val configs = artifact.configurations
          val sourceDirectories: Seq[File] = configs.toList.flatMap { configRef =>
            (project / sbt.Configuration.of(configRef.name.capitalize, configRef.name) / Keys.sourceDirectories).get(settingsData.value).toList.flatten
          }
          sourceDirectories
        }
      }

      // Download and add transitive sources from the classpath
      val classiferArtifacts: Seq[(ModuleID, Artifact, File)] = updateClassifiers.value.configurations.flatMap(_.details.flatMap(_.modules.flatMap(report => report.artifacts.map(x => (report.module, x._1, x._2)))))
      val sourceClassiferArtifacts = classiferArtifacts.filter(tuple => tuple._2.classifier.contains("sources") && dependencyModuleIds.contains(tuple._1))

      val externalSources = sourceClassiferArtifacts.map(_._3)
      val internalAndExternalSources = sourceDirectories.value ++ (javaHomeSrc +: (transitiveSourceDirectories ++ transitiveSourceDirectories2).distinct) ++ externalSources
      props.put("Sources", internalAndExternalSources.map(_.getAbsolutePath).mkString(","))
      val baseDir = baseDirectory.value
      val lastLogDir = Keys.forkOptions.value.workingDirectory match {
        case Some(dir) => dir
        case _=> baseDir
      }
      props.put("LastLogDir", lastLogDir.getAbsolutePath)
      props
    },

    jitwatchConfigFile := {
      val jitwatchProps = target.value / ("jitwatch-" + configuration.value.name + ".properties")
      val hotSpotLog = target.value / "hotspot.log"
      val log = streams.value.log
      val fw = new FileWriter(jitwatchProps)
      try {
        jitwatchConfigFileContents.value.store(fw, null)
        // TODO figure out the last benchmark that was run and focus the UI on that member with: -Djitwatch.focus.member="scala/collection/mutable/ArrayOpsBenchmark insertInteger (Lorg/openjdk/jmh/infra/Blackhole;)V"
        log.info(s"^-- UNRESOLVED DEPENDENCIES warnings above are normal, please ignore")
        log.info("After cloning https://github.com/AdoptOpenJDK/jitwatch to $JITWATCH_HOME, compile and launch with:")
        log.info(s"mvn -f $$JITWATCH_HOME clean compile exec:java -Djitwatch.config.file=${jitwatchProps.getAbsolutePath} -Djitwatch.logfile=${hotSpotLog.getAbsolutePath}")
        log.info("Note: Add, for example, `-Djitwatch.focus.member=\"scala/collection/mutable/ArrayOpsBenchmark insertInteger (Lorg/openjdk/jmh/infra/Blackhole;)V\"` to focus UI on a method of interest on startup.")
      } finally {
        fw.close()
      }
    }    
  )
}
