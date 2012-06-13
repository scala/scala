import sbt._
import Keys._

object ScalaBuildKeys {
  val lockerLock: TaskKey[Unit] = TaskKey("locker-lock", 
    "Locks the locker layer of the compiler build such that it won't rebuild on changed source files.")
  val lockerUnlock: TaskKey[Unit] = TaskKey("locker-unlock", 
    "Unlocks the locker layer of the compiler so that it will be recompiled on changed source files.")
  val lockFile: SettingKey[File] = SettingKey("lock-file", 
    "Location of the lock file compiling this project.")
  // New tasks/settings specific to the scala build.
  val lock: TaskKey[Unit] = TaskKey("lock", "Locks this project so it won't be recompiled.")
  val unlock: TaskKey[Unit] = TaskKey("unlock", "Unlocks this project so it will be recompiled.")
  val makeDist: TaskKey[File] = TaskKey("make-dist", 
    "Creates a mini-distribution (scala home directory) for this build in a zip file.")
  val makeExplodedDist: TaskKey[File] = TaskKey("make-exploded-dist", 
    "Creates a mini-distribution (scala home directory) for this build in a directory.")
  val makeDistMappings: TaskKey[Map[File, String]] = TaskKey("make-dist-mappings", 
    "Creates distribution mappings for creating zips,jars,directorys,etc.")
  val buildFixed = AttributeKey[Boolean]("build-uri-fixed")

}
