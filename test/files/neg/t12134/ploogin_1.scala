
package t12134

import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{Plugin, PluginComponent}

//import java.nio.file.Files.createFile
//import java.nio.file.Files.deleteIfExists
import java.nio.file.Paths
//import java.nio.file.attribute.PosixFilePermissions.asFileAttribute
//import java.nio.file.attribute.PosixFilePermission.OWNER_READ
//import java.util.EnumSet

/** A test plugin.  */
class Unplugged(val global: Global) extends Plugin {
  import global._

  val name = "unplugged"
  val description = "A plugin that creates local output before jvm writes classes."
  val components = List[PluginComponent](TestComponent)

  private object TestComponent extends PluginComponent {
    val global: Unplugged.this.global.type = Unplugged.this.global
    override val runsBefore = List("jvm")
    val runsAfter = List("typer")
    val phaseName = Unplugged.this.name
    override def description = "Interfere with classwriter!"
    def newPhase(prev: Phase) = new TestPhase(prev)
    class TestPhase(prev: Phase) extends StdPhase(prev) {
      override def description = TestComponent.this.description
      def apply(unit: CompilationUnit): Unit = {
        global.settings.outdir.value = s"${global.settings.plugin.value.head}/${global.settings.outdir.value}"
        val path = Paths.get(global.settings.outdir.value)
        val file = path.toFile()
        //import scala.util.Try
        //val perms = asFileAttribute(EnumSet.of(OWNER_READ))
        //val res = Try(createFile(path, perms))
        //assert(res.isSuccess)
        file.delete()
        assert(file.createNewFile() && file.setReadOnly())
      }
    }
  }
}
