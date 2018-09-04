package scala.reflect.internal.jpms;

import javax.tools.JavaCompiler;
import javax.tools.StandardJavaFileManager;
import javax.tools.StandardLocation;
import javax.tools.ToolProvider;
import java.lang.module.ModuleFinder;
import java.nio.charset.Charset;
import java.util.Locale;
import java.util.function.Consumer;

/** Use the standard library of the current JVM. */
final class StandardClassAndModulePath extends ModuleFinderAndFileManager {
    private StandardJavaFileManager fileManager;

    StandardClassAndModulePath(Consumer<StandardJavaFileManager> optionSetter) {
        JavaCompiler systemJavaCompiler = ToolProvider.getSystemJavaCompiler();
        fileManager = systemJavaCompiler.getStandardFileManager(new NullDiagnosticListener<>(), Locale.getDefault(), Charset.defaultCharset());
        optionSetter.accept(fileManager);
    }

    public ModuleFinder moduleFinder() {
        return ModuleFinder.compose(
                ModuleFinder.ofSystem(),
                locationToFinder(fileManager, StandardLocation.CLASS_OUTPUT),
                locationToFinder(fileManager, StandardLocation.MODULE_PATH)
        );
    }

    @Override
    public StandardJavaFileManager fileManager() {
        return fileManager;
    }

}
