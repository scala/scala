package scala.reflect.classloading;

public class MustLoadedFirst {
	static {
		LoadingOrder.loadedClasses.add(MustLoadedFirst.class);
	}
}