Assertion failed while re-compiling a package object containing a case class
Reported by: 	rjm 	Owned by: 	odersky
Priority: 	normal 	Component: 	Compiler
Keywords: 		Cc: 	erik.engbrecht@…
Fixed in version:
Description

While playing with the latest 2.8 snapshot (r18215-b20090706020217 to be precise) I noticed that a package object containing a case class gets an assertion failure when re-compiling (i.e., the class files already exist from a previous run, even a previous build of the exact same source). A minimal test case:

package object foo {

    case class X()

}

And the error:

Exception in thread "main" java.lang.AssertionError?: assertion failed: List(object package$X, object package$X)

    at scala.Predef$.assert(Predef.scala:97) at scala.tools.nsc.symtab.Symbols$Symbol.suchThat(Symbols.scala:988) at scala.tools.nsc.symtab.Symbols$Symbol.linkedModuleOfClass(Symbols.scala:1159) at scala.tools.nsc.backend.jvm.GenJVM$BytecodeGenerator?.genClass(GenJVM.scala:203) at scala.tools.nsc.backend.jvm.GenJVM$JvmPhase?$$anonfun$run$2.apply(GenJVM.scala:50) at scala.tools.nsc.backend.jvm.GenJVM$JvmPhase?$$anonfun$run$2.apply(GenJVM.scala:50) at scala.collection.Iterator$class.foreach(Iterator.scala:500) at scala.collection.generic.MapTemplate?$$anon$4.foreach(MapTemplate?.scala:156) at scala.tools.nsc.backend.jvm.GenJVM$JvmPhase?.run(GenJVM.scala:50) at scala.tools.nsc.Global$Run.compileSources(Global.scala:781) at scala.tools.nsc.Global$Run.compile(Global.scala:855) at scala.tools.nsc.Main$.process(Main.scala:75) at scala.tools.nsc.Main$.main(Main.scala:89) at scala.tools.nsc.Main.main(Main.scala)

A normal class does not cause the failure; nor does a case object.
Attachments
Change History
Changed 4 weeks ago by eengbrec

    * cc erik.engbrecht@… added

Changed 3 weeks ago by rompf

    * owner changed from scala_reviewer to scala_meeting

Changed 3 weeks ago by rompf

    * owner changed from scala_meeting to odersky

Add/Change #2130 (Assertion failed while re-compiling a package object containing a case class)
Comment (you may use WikiFormatting here):

Change Properties
Summary:
Reporter:
Description: 	While playing with the latest 2.8 snapshot (r18215-b20090706020217 to be precise) I noticed that a package object containing a case class gets an assertion failure when re-compiling (i.e., the class files already exist from a previous run, even a previous build of the exact same source). A minimal test case: package object foo { case class X() } And the error: Exception in thread "main" java.lang.AssertionError: assertion failed: List(object package$X, object package$X) at scala.Predef$.assert(Predef.scala:97) at scala.tools.nsc.symtab.Symbols$Symbol.suchThat(Symbols.scala:988) at scala.tools.nsc.symtab.Symbols$Symbol.linkedModuleOfClass(Symbols.scala:1159) at scala.tools.nsc.backend.jvm.GenJVM$BytecodeGenerator.genClass(GenJVM.scala:203) at scala.tools.nsc.backend.jvm.GenJVM$JvmPhase$$anonfun$run$2.apply(GenJVM.scala:50) at scala.tools.nsc.backend.jvm.GenJVM$JvmPhase$$anonfun$run$2.apply(GenJVM.scala:50) at scala.collection.Iterator$class.foreach(Iterator.scala:500) at scala.collection.generic.MapTemplate$$anon$4.foreach(MapTemplate.scala:156) at scala.tools.nsc.backend.jvm.GenJVM$JvmPhase.run(GenJVM.scala:50) at scala.tools.nsc.Global$Run.compileSources(Global.scala:781) at scala.tools.nsc.Global$Run.compile(Global.scala:855) at scala.tools.nsc.Main$.process(Main.scala:75) at scala.tools.nsc.Main$.main(Main.scala:89) at scala.tools.nsc.Main.main(Main.scala) A normal class does not cause the failure; nor does a case object.
Type:
