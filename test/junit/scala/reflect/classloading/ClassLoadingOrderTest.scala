package scala.reflect.classloading


import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.reflect.runtime.universe._


@RunWith(classOf[JUnit4])
class Launcher {
	@Test def testLoadingOrder(){
		//The computation of types should not lead to the loading of classes
		typeOf[SomeClass[MustLoadedSecond]]

		Class.forName(classOf[MustLoadedFirst].getName, true, getClass.getClassLoader)
		Class.forName(classOf[MustLoadedSecond].getName, true, getClass.getClassLoader)

		assert(LoadingOrder.loadedClasses.size == 2)
		assert(LoadingOrder.loadedClasses.get(0) == classOf[MustLoadedFirst])
		assert(LoadingOrder.loadedClasses.get(1) == classOf[MustLoadedSecond])
	}
}

class  SomeClass[T <: MustLoadedSecond]
