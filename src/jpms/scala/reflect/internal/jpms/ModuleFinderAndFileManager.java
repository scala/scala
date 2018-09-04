package scala.reflect.internal.jpms;

import javax.tools.StandardJavaFileManager;
import javax.tools.StandardLocation;
import java.lang.module.ModuleFinder;
import java.nio.file.*;
import java.util.*;
import java.util.function.Consumer;

/** A configured module finder and java file manager */
abstract class ModuleFinderAndFileManager {
    public static ModuleFinderAndFileManager get(Optional<String> release, Consumer<StandardJavaFileManager> optionSetter) {
        if (release.isPresent()) return new CtSymClassAndModulePath(release.get(), optionSetter);
        else return new StandardClassAndModulePath(optionSetter);
    }

    public abstract ModuleFinder moduleFinder();

    public abstract StandardJavaFileManager fileManager();

    static ModuleFinder locationToFinder(StandardJavaFileManager fileManager, StandardLocation location) {
        Iterable<? extends Path> locationAsPaths = fileManager.getLocationAsPaths(location);
        if (locationAsPaths == null) {
            return ModuleFinder.of();
        }
        ArrayList<Path> paths = new ArrayList<>();
        for (Path locationAsPath : locationAsPaths) {
            paths.add(locationAsPath);
        }
        return ModuleFinder.of(paths.toArray(new Path[0]));
    }
}
