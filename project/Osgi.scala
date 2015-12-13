import aQute.lib.osgi.Builder
import aQute.lib.osgi.Constants._
import java.util.Properties
import sbt._
import sbt.Keys._
import scala.collection.JavaConversions._
import VersionUtil.versionProperties

/** OSGi packaging for the Scala build, distilled from sbt-osgi. We do not use sbt-osgi because it
  * depends on a newer version of BND which gives slightly different output (probably OK to upgrade
  * in the future but for now it would make comparing the sbt and ant build output harder) and does
  * not allow a crucial bit of configuration that we need: Setting the classpath for BND. In sbt-osgi
  * this is always `fullClasspath in Compile` whereas we want `products in Compile in packageBin`. */
object Osgi {
  val bundle = TaskKey[File]("osgiBundle", "Create an OSGi bundle.")
  val bundleName = SettingKey[String]("osgiBundleName", "The Bundle-Name for the manifest.")
  val bundleSymbolicName = SettingKey[String]("osgiBundleSymbolicName", "The Bundle-SymbolicName for the manifest.")
  val headers = SettingKey[Seq[(String, String)]]("osgiHeaders", "Headers and processing instructions for BND.")

  def settings: Seq[Setting[_]] = Seq(
    bundleName := description.value,
    bundleSymbolicName := organization.value + "." + name.value,
    headers := {
      val v = VersionUtil.versionProperties.value.osgiVersion
      Seq(
        "Bundle-Name" -> bundleName.value,
        "Bundle-SymbolicName" -> bundleSymbolicName.value,
        "ver" -> v,
        "Export-Package" -> ("*;version=${ver}"),
        "Import-Package" -> ("scala.*;version=\"${range;[==,=+);${ver}}\",*"),
        "Bundle-Version" -> v,
        "Bundle-RequiredExecutionEnvironment" -> "JavaSE-1.6, JavaSE-1.7",
        "-eclipse" -> "false"
      )
    },
    bundle <<= Def.task {
      bundleTask(headers.value.toMap, (products in Compile in packageBin).value,
        (artifactPath in (Compile, packageBin)).value, Nil, streams.value)
    },
    packagedArtifact in (Compile, packageBin) <<= (artifact in (Compile, packageBin), bundle).identityMap,
    // Also create OSGi source bundles:
    artifact in (Compile, packageBin) ~= (_.copy(`type` = "bundle")),
    packageOptions in (Compile, packageSrc) += Package.ManifestAttributes(
      "Bundle-Name" -> (description.value + " Sources"),
      "Bundle-SymbolicName" -> (bundleSymbolicName.value + ".source"),
      "Bundle-Version" -> versionProperties.value.osgiVersion,
      "Eclipse-SourceBundle" -> (bundleSymbolicName.value + ";version=\"" + versionProperties.value.osgiVersion + "\";roots:=\".\"")
    )
  )

  def bundleTask(headers: Map[String, String], fullClasspath: Seq[File], artifactPath: File,
                 resourceDirectories: Seq[File], streams: TaskStreams): File = {
    val log = streams.log
    val builder = new Builder
    builder.setClasspath(fullClasspath.toArray)
    headers foreach { case (k, v) => builder.setProperty(k, v) }
    val includeRes = resourceDirectories.filter(_.exists).map(_.getAbsolutePath).mkString(",")
    if(!includeRes.isEmpty) builder.setProperty(INCLUDERESOURCE, includeRes)
    builder.getProperties.foreach { case (k, v) => log.debug(s"bnd: $k: $v") }
    // builder.build is not thread-safe because it uses a static SimpleDateFormat.  This ensures
    // that all calls to builder.build are serialized.
    val jar = synchronized { builder.build }
    builder.getWarnings.foreach(s => log.warn(s"bnd: $s"))
    builder.getErrors.foreach(s => log.error(s"bnd: $s"))
    IO.createDirectory(artifactPath.getParentFile)
    jar.write(artifactPath)
    artifactPath
  }
}
