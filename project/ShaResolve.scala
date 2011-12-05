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
  

  def settings: Seq[Setting[_]] = Seq(
    pullBinaryLibs in ThisBuild <<= (baseDirectory, streams) map resolveLibs
  )

  def resolveLibs(dir: File, s: TaskStreams): Unit = {
     for {
       (file, name) <- dir ** "*.desired.sha1" x relativeTo(dir)
       uri = name.dropRight(13)       
       jar = dir / uri
       if !jar.exists || !isValidSha(file)
       sha = getShaFromShafile(file)
     } pullFile(jar, sha + "/" + uri, s)
  }

  def getShaFromShafile(file: File): String = (IO read file split "\\s" headOption) getOrElse error("No SHA found for " + file)


  def isValidSha(file: File): Boolean =
    try (Process(Seq("shasum", "-p", "--check", file.getAbsolutePath), Some(file.getParentFile)).!!  contains "OK") 
    catch {
      case t: Exception => false
    }
     

  def pullFile(file: File, uri: String, s: TaskStreams): Unit = {
    val url = remote_urlbase + "/" + uri
    val fous = new java.io.FileOutputStream(file)
    s.log.info("Pulling [" + url + "] to [" + file + "]")
    try Http(dispatch.url(url) >>> fous) finally fous.close()
  }
  
  def pushFile(file: File, uri: String, user: String, pw: String): Unit = {
    val url = remote_urlbase + "/" + uri
    val sender = dispatch.url(url).PUT.as(user,pw) <<< (file, "application/java-archive")
    // TODO - output to logger.
    Http(sender >>> System.out)
  }
}
