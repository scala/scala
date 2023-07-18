package scala.build

import aQute.bnd.osgi.Builder
import aQute.bnd.osgi.Constants._
import java.util.jar.Attributes
import sbt.{License => _, _}
import sbt.Keys._
import collection.JavaConverters._
import VersionUtil.versionProperties

/** OSGi packaging for the Scala build, distilled from sbt-osgi.
 *
 * We don't use sbt-osgi (yet) because it does not allow a crucial bit of
  * configuration that we need: Setting the classpath for BND. In sbt-osgi this is always
  * `fullClasspath in Compile` whereas we want `products in Compile in packageBin`. */
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

        // bnd 3.0 fixes for https://github.com/bndtools/bnd/issues/971. This changes our OSGi
        // metadata by adding Import-Package automatically for all of our exported packages.
        // Supposedly this is the right thing to do: https://blog.osgi.org/2007/04/importance-of-exporting-nd-importing.html
        // but I'm disabling the feature (`-noimport:=true`) to avoid changing this detail of
        // our little understood OSGi metadata for now.
        "Export-Package" -> "*;version=${ver};-noimport:=true;-split-package:=merge-first",

        "Import-Package" -> raw"""scala.*;version="$${range;[==,=+);$${ver}}",*""",
        "Bundle-Version" -> v,
        "Bundle-RequiredExecutionEnvironment" -> "JavaSE-1.8",
        "-eclipse" -> "false",

        // Great new feature in modern bnd versions: reproducible builds.
        // Omits the Bundle-LastModified header and avoids using System.currentTimeMillis
        // for ZIP metadata.
        "-reproducible" -> "true",

        // https://github.com/bndtools/bnd/commit/2f1d89428559d21857b87b6d5b465a18a300becc (bndlib 4.2.0)
        // seems to have fixed a bug in its detection class references in Class.forName("some.Class")
        // For our build, this adds an import on the package "com.cloudius.util" (referred to by an optional
        // part of JLine. This directive disables the Class.forName scanning. An alternative fix would be
        // direct this to be an optional dependency (as we do for jline itself with `"Import-Package" -> ("jline.*;resolution:=optional," + ... )`)
        "-noclassforname" -> "true" //
      )
    },
    jarlist := false,
    bundle := Def.task {
      val cp = (Compile / packageBin / products).value
      val licenseFiles = License.licenseMapping.value.map(_._1)
      bundleTask(headers.value.toMap, jarlist.value, cp,
        (Compile / packageBin / artifactPath).value, cp ++ licenseFiles, streams.value)
    }.value,
    Compile / packageBin / packagedArtifact := (((Compile / packageBin / artifact).value, bundle.value)),
    // Also create OSGi source bundles:
    Compile / packageSrc / packageOptions += Package.ManifestAttributes(
      "Bundle-Name" -> (description.value + " Sources"),
      "Bundle-SymbolicName" -> (bundleSymbolicName.value + ".source"),
      "Bundle-Version" -> versionProperties.value.osgiVersion,
      "Eclipse-SourceBundle" -> (bundleSymbolicName.value + ";version=\"" + versionProperties.value.osgiVersion + "\";roots:=\".\"")
    ),
    Keys.`package` := bundle.value
  )

  def bundleTask(headers: Map[String, String], jarlist: Boolean, fullClasspath: Seq[File], artifactPath: File,
                 resourceDirectories: Seq[File], streams: TaskStreams): File = {
    val log = streams.log
    val builder = new Builder
    builder.setClasspath(fullClasspath.toArray)
    headers foreach { case (k, v) => builder.setProperty(k, v) }

    // https://github.com/scala/scala-dev/issues/254
    // Must be careful not to include scala-asm.jar within scala-compiler.jar!
    def resourceDirectoryRef(f: File) = (if (f.getName endsWith ".jar") "@" else "") + f.getAbsolutePath

    val includeRes = resourceDirectories.filter(_.exists).map(resourceDirectoryRef).mkString(",")
    if (includeRes.nonEmpty) builder.setProperty(INCLUDERESOURCE, includeRes)
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
