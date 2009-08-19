package xsbt

import java.io.File
import org.specs.Specification

object CheckBasic extends Specification
{
	val basicName = new File("Basic.scala")
	val basicSource = "package org.example { object Basic }"

	"Compiling basic file should succeed" in {
		WithFiles(basicName -> basicSource){ files =>
			TestCompile(files){ loader => Class.forName("org.example.Basic", false, loader) }
			true must be(true) // don't know how to just check that previous line completes without exception
		}
	}
	"Analyzer plugin should send source begin and end" in {
		WithFiles(basicName -> basicSource) { files =>
			CallbackTest(files) { callback =>
				(callback.beganSources) must haveTheSameElementsAs(files)
				(callback.endedSources) must haveTheSameElementsAs(files)
			}
		}
	}
}