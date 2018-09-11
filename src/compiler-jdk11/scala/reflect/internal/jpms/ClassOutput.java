package scala.reflect.internal.jpms;

import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.StandardLocation;
import java.io.IOException;
import java.lang.module.ModuleFinder;
import java.lang.module.ModuleReference;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.Set;
import java.util.function.Supplier;

public abstract class ClassOutput {
    public void configure(StandardJavaFileManager fileManager) {
    }

    public abstract ModuleFinder moduleFinder(StandardJavaFileManager fileManager) throws IOException;

    public static class OutputPathClassOutput extends ClassOutput {
        private Path path;

        public OutputPathClassOutput(Path path) {
            this.path = path;
        }

        @Override
        public void configure(StandardJavaFileManager fileManager) {
            try {
                fileManager.setLocationFromPaths(StandardLocation.CLASS_OUTPUT, Collections.singleton(path));
            } catch (IOException ex) {
                throw new RuntimeException(ex);
            }
        }

        @Override
        public ModuleFinder moduleFinder(StandardJavaFileManager fileManager) throws IOException {
            // Consider a module-info.class from the class output directory if there is not module-info.java among the compiled sources.
            JavaFileObject module = fileManager.getJavaFileForInput(StandardLocation.CLASS_OUTPUT, "module-info", JavaFileObject.Kind.CLASS);
            if (module != null) {
                ModuleFinder classOutputModuleFinder = ModuleFinder.of(path);
                Set<ModuleReference> classOutputModules = classOutputModuleFinder.findAll();
                if (classOutputModules.size() != 1) {
                    throw new IllegalStateException("Expected one module-info.class in " + path);
                }
                return classOutputModuleFinder;
            } else return null;
        }
    }

    public static class SupplierClassOutput extends ClassOutput {
        private Supplier<byte[]> supplier;

        public SupplierClassOutput(Supplier<byte[]> supplier) {
            this.supplier = supplier;
        }

        @Override
        public ModuleFinder moduleFinder(StandardJavaFileManager fileManager) throws IOException {
            byte[] contents = supplier.get();
            if (contents == null) return null;
            // Annoyingly, I can't find a way to load a module-info.class file with `ModuleFinder.of`
            // without writing into to a NIO Path directory.
            // TODO JPMS This cleanup up this temp directory.
            Path tempDirectory = Files.createTempDirectory("suppler-class-output-");
            Files.write(tempDirectory.resolve("module-info.class"), contents);
            return ModuleFinder.of(tempDirectory);
        }
    }
}
