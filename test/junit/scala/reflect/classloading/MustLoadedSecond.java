package scala.reflect.classloading;

public class MustLoadedSecond {
	static {
		LoadingOrder.loadedClasses.add(MustLoadedSecond.class);
	}
}
