package scala.reflect.internal.jpms;

import javax.tools.JavaFileManager;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Set;

public final class FileManagerJava9Api {
    private FileManagerJava9Api() {
    }

    public static Iterable<? extends Path> getLocationAsPaths(StandardJavaFileManager self, JavaFileManager.Location location) {
        return self.getLocationAsPaths(location);
    }

    public static Iterable<Set<JavaFileManager.Location>> listLocationsForModules(StandardJavaFileManager self, JavaFileManager.Location location) throws IOException {
        return self.listLocationsForModules(location);
    }

    public static Path asPath(StandardJavaFileManager self, JavaFileObject fileObject) {
        return self.asPath(fileObject);
    }

    public static String inferModuleName(StandardJavaFileManager self, JavaFileManager.Location location) throws IOException {
        return self.inferModuleName(location);
    }

    public static JavaFileManager.Location getLocationForModule(StandardJavaFileManager self, JavaFileManager.Location location, JavaFileObject fileObject) throws IOException {
        return self.getLocationForModule(location, fileObject);
    }
    public static JavaFileManager.Location getLocationForModule(StandardJavaFileManager self, JavaFileManager.Location location, String moduleName) throws IOException {
        return self.getLocationForModule(location, moduleName);
    }

    public static Iterable<? extends JavaFileObject> getJavaFileObjects(StandardJavaFileManager self, Path... paths) throws IOException {
        return self.getJavaFileObjects(paths);
    }
    public static boolean isModuleOrientedLocation(JavaFileManager.Location location) {
        return location.isModuleOrientedLocation();
    }
}
