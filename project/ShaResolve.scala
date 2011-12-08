import sbt._

import Build._
import Keys._
import Project.Initialize
import scala.collection.{ mutable, immutable }




/** Helpers to resolve SHA artifacts from typesafe repo. */
object ShaResolve {
  import dispatch.{Http,url}
  val remote_urlbase="http://typesafe.artifactoryonline.com/typesafe/scala-sha-bootstrap/org/scala-lang/bootstrap"  
  
  val pullBinaryLibs = TaskKey[Unit]("pull-binary-libs", "Pulls binary libs by the SHA key.")
  val pushBinaryLibs = TaskKey[Unit]("push-binary-libs", "Pushes binary libs whose SHA has changed.")
  val binaryLibCache = SettingKey[File]("binary-lib-cache", "Location of the cache of binary libs for this scala build.")

  def settings: Seq[Setting[_]] = Seq(
    binaryLibCache in ThisBuild := file(System.getProperty("user.home")) / ".sbt" / "cache" / "scala",
    pullBinaryLibs in ThisBuild <<= (baseDirectory, binaryLibCache, streams) map resolveLibs
  )

  def resolveLibs(dir: File, cacheDir: File, s: TaskStreams): Unit = {
     for {
       (file, name) <- dir ** "*.desired.sha1" x relativeTo(dir)
       uri = name.dropRight(13)       
       jar = dir / uri
       if !jar.exists || !isValidSha(file)
       sha = getShaFromShafile(file)
     } pullFile(jar, sha + "/" + uri, cacheDir, s)
  }

  def getShaFromShafile(file: File): String = (IO read file split "\\s" headOption) getOrElse error("No SHA found for " + file)


  def isValidSha(file: File): Boolean =
    try (Process(Seq("shasum", "-p", "--check", file.getAbsolutePath), Some(file.getParentFile)).!!  contains "OK") 
    catch {
      case t: Exception => false
    }
     

  def pullFile(file: File, uri: String, cacheDir: File, s: TaskStreams): Unit = {
    val cachedFile = cacheDir / uri
    if (!cachedFile.exists) {
      // Ensure the directory for the cache exists.
      cachedFile.getParentFile.mkdirs()
      val url = remote_urlbase + "/" + uri
      val fous = new java.io.FileOutputStream(cachedFile)
      s.log.info("Pulling [" + cachedFile + "] to cache")
      try Http(dispatch.url(url) >>> fous) finally fous.close()
    }
    s.log.info("Pulling [" + file + "] from local cache")
    IO.copyFile(cachedFile, file)
  }
  
  def pushFile(file: File, uri: String, user: String, pw: String): Unit = {
    val url = remote_urlbase + "/" + uri
    val sender = dispatch.url(url).PUT.as(user,pw) <<< (file, "application/java-archive")
    // TODO - output to logger.
    Http(sender >>> System.out)
  }
}
