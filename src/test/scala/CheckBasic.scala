package xsbt

import java.io.File
import org.specs.Specification

object CheckBasic extends Specification
{
	val basicName = new File("Basic.scala")
	val basicSource = "package org.example { object Basic }"
	
	val mainName = new File("Main.scala")
	val mainSource = "object Main { def main(args: Array[String]) {} }"
	
	val super1Name = new File("a/Super.scala")
	val super2Name = new File("a/Super2.scala")
	val midName = new File("b/Middle.scala")
	val sub1Name = new File("b/SubA.scala")
	val sub2Name = new File("b/SubB.scala")
	val sub3Name = new File("SubC.scala")
	val super1Source = "package a; trait Super"
	val super2Source = "class Super2"
	val midSource = "package y.w; trait Mid extends a.Super"
	val subSource1 = "package a; trait Sub1 extends y.w.Mid"
	val subSource2 = "trait Sub2 extends a.Super"
	val subSource3 = "private class F extends a.Super; package c { object Sub3 extends Super2 }"
		
	"Compiling basic file should succeed" in {
		WithFiles(basicName -> basicSource){ files =>
			TestCompile(files){ loader => Class.forName("org.example.Basic", false, loader) }
		}
	}
	
	"Analysis plugin" should {
		"send source begin and end" in {
			WithFiles(basicName -> basicSource) { files =>
				CallbackTest(files) { callback =>
					(callback.beganSources) must haveTheSameElementsAs(files)
					(callback.endedSources) must haveTheSameElementsAs(files)
				}
			}
		}
	
		"detect applications" in {
			WithFiles(mainName -> mainSource ) { files =>
				CallbackTest(files) { callback =>
					(callback.applications) must haveTheSameElementsAs(files.map(file => (file, "Main")))
				}
			}
		}
		
		"detect subclasses" in {
			WithFiles(super1Name -> super1Source, midName -> midSource, sub1Name -> subSource1, sub2Name -> subSource2,
				super2Name -> super2Source, sub3Name -> subSource3)
			{
				case files @ Seq(supFile, midFile, sub1File, sub2File, sup2File, sub3File) =>
					CallbackTest(files,Seq( "a.Super", "Super2", "x.Super3")) { (callback, ignore, ignore2) =>
						val expected = (sub1File, "a.Super", "a.Sub1", false) :: (sub2File, "a.Super", "a.Sub2", false) ::
							(sub3File, "Super2", "Sub3", true) :: Nil
						//println(callback.foundSubclasses)
						//println(callback.invalidSuperclasses)
						(callback.foundSubclasses) must haveTheSameElementsAs(expected)
						(callback.invalidSuperclasses) must haveTheSameElementsAs(Seq("x.Super3"))
					}
			}
		}
	}
}