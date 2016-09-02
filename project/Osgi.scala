import aQute.bnd.osgi.Builder
import aQute.bnd.osgi.Constants._
import java.util.Properties
import java.util.jar.Attributes
import sbt._
import sbt.Keys._
import collection.JavaConverters._
import VersionUtil.versionProperties

/** OSGi packaging for the Scala build, distilled from sbt-osgi. We do not use sbt-osgi because it
  * depends on a newer version of BND which gives slightly different output (probably OK to upgrade
  * in the future, now that the Ant build has been removed) and does not allow a crucial bit of
  * configuration that we need: Setting the classpath for BND. In sbt-osgi this is always
  *  `fullClasspath in Compile` whereas we want `products in Compile in packageBin`. */
object Osgi {
  val bundle = TaskKey[File]("osgiBundle", "Create an OSGi bundle.")
  val bundleName = SettingKey[String]("osgiBundleName", "The Bundle-Name for the manifest.")
  val bundleSymbolicName = SettingKey[String]("osgiBundleSymbolicName", "The Bundle-SymbolicName for the manifest.")
  val headers = SettingKey[Seq[(String, String)]]("osgiHeaders", "Headers and processing instructions for BND.")
  val jarlist = SettingKey[Boolean]("osgiJarlist", "List classes in manifest.")

  def settings: Seq[Setting[_]] = Seq(
    bundleName := description.value,
    bundleSymbolicName := organization.value + "." + name.value,
    headers := {
      val v = VersionUtil.versionProperties.value.osgiVersion
      Seq(
        "Bundle-Name" -> bundleName.value,
        "Bundle-SymbolicName" -> bundleSymbolicName.value,
        "ver" -> v,
        "Export-Package" -> "*;version=${ver};-split-package:=merge-first",
        "Import-Package" -> "scala.*;version=\"${range;[==,=+);${ver}}\",*",
        "Bundle-Version" -> v,
        "Bundle-RequiredExecutionEnvironment" -> "JavaSE-1.8",
        "-eclipse" -> "false"
      )
    },
    jarlist := false,
    bundle <<= Def.task {
      val res = (products in Compile in packageBin).value
      bundleTask(headers.value.toMap, jarlist.value, (products in Compile in packageBin).value,
        (artifactPath in (Compile, packageBin)).value, res, streams.value)
    },
    packagedArtifact in (Compile, packageBin) <<= (artifact in (Compile, packageBin), bundle).identityMap,
    // Also create OSGi source bundles:
    packageOptions in (Compile, packageSrc) += Package.ManifestAttributes(
      "Bundle-Name" -> (description.value + " Sources"),
      "Bundle-SymbolicName" -> (bundleSymbolicName.value + ".source"),
      "Bundle-Version" -> versionProperties.value.osgiVersion,
      "Eclipse-SourceBundle" -> (bundleSymbolicName.value + ";version=\"" + versionProperties.value.osgiVersion + "\";roots:=\".\"")
    )
  )

  def bundleTask(headers: Map[String, String], jarlist: Boolean, fullClasspath: Seq[File], artifactPath: File,
                 resourceDirectories: Seq[File], streams: TaskStreams): File = {
    val log = streams.log
    val builder = new Builder
    builder.setClasspath(fullClasspath.toArray)
    headers foreach { case (k, v) => builder.setProperty(k, v) }
    val includeRes = resourceDirectories.filter(_.exists).map(_.getAbsolutePath).mkString(",")
    if(!includeRes.isEmpty) builder.setProperty(INCLUDERESOURCE, includeRes)
    builder.getProperties.asScala.foreach { case (k, v) => log.debug(s"bnd: $k: $v") }
    // builder.build is not thread-safe because it uses a static SimpleDateFormat.  This ensures
    // that all calls to builder.build are serialized.
    val jar = synchronized { builder.build }
    builder.getWarnings.asScala.foreach(s => log.warn(s"bnd: $s"))
    builder.getErrors.asScala.foreach(s => log.error(s"bnd: $s"))
    IO.createDirectory(artifactPath.getParentFile)
    if (jarlist) {
      val entries = jar.getManifest.getEntries
      for ((name, resource) <- jar.getResources.asScala if name.endsWith(".class")) {
        entries.put(name, new Attributes)
      }
    }
    jar.write(artifactPath)
    artifactPath
  }
}
