package xsbt

import java.io.File
import org.specs.Specification

object CheckBasic extends Specification
{
	"Compiling basic file should succeed" in {
		val name = new File("Basic.scala")
		WithFiles( name -> "package org.example { object Basic }" ){ files =>
			TestCompile(files){ loader => Class.forName("org.example.Basic", false, loader) }
		}
	}
	
	"Analysis plugin" should {
		"send source begin and end" in {
			val name = new File("Basic.scala")
			WithFiles(name -> "object Basic" ) { files =>
				CallbackTest(files) { callback =>
					(callback.beganSources) must haveTheSameElementsAs(files)
					(callback.endedSources) must haveTheSameElementsAs(files)
				}
			}
		}
	
		"detect applications" in {
			val name = new File("Main.scala")
			WithFiles(name -> "object Main { def main(args: Array[String]) {} }" ) { files =>
				CallbackTest(files) { callback =>
					println(callback.applications)
					(callback.applications) must haveTheSameElementsAs(files.map(file => (file, "Main")))
				}
			}
		}
	}
}