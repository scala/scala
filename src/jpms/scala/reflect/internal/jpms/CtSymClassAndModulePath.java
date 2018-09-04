package scala.reflect.internal.jpms;

import javax.tools.JavaCompiler;
import javax.tools.StandardJavaFileManager;
import javax.tools.StandardLocation;
import javax.tools.ToolProvider;
import java.io.IOException;
import java.lang.module.ModuleFinder;
import java.net.URI;
import java.nio.charset.Charset;
import java.nio.file.*;
import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;

/** Support for <a href="http://openjdk.java.net/jeps/247">JEP 247: Compile for Older Platform Versions - OpenJDK</a>
 *  and <a href="http://openjdk.java.net/jeps/238">JEP 238: Multi-Release JAR Files</a> */
final class CtSymClassAndModulePath extends ModuleFinderAndFileManager {
    private String release;
    private FileSystem fileSystem = getCtSymFileSystem();
    private StandardJavaFileManager fileManager;
    private HashMap<String, Path> includedSysModuleNames = new HashMap<>();

    CtSymClassAndModulePath(String release, Consumer<StandardJavaFileManager> optionSetter) {
        super();
        this.release = release;
        init(optionSetter);
    }

    private void init(Consumer<StandardJavaFileManager> optionSetter) {
        JavaCompiler systemJavaCompiler = ToolProvider.getSystemJavaCompiler();
        fileManager = systemJavaCompiler.getStandardFileManager(new NullDiagnosticListener<>(), Locale.getDefault(), Charset.defaultCharset());

        // Hidden option in the FILEMANAGER group that lets us control which parts of multi-release JARs are served up
        fileManager.handleOption("--multi-release", List.of(release).iterator());

        try {
            int releaseInt = Integer.parseInt(release);
            String releaseCode = "" + (releaseInt <= 9 ? release.charAt(0) : (char) ('A' + (releaseInt - 10)));

            Path root = fileSystem.getRootDirectories().iterator().next();
            List<Path> platformPath = new ArrayList<>();
            // JDK9 ct.sym contains a file `N/system-modules` with the list of modules that contribute to the
            // JDK7/8 platform class path.
            List<Path> versionDirs = Files.list(root).collect(Collectors.toList());
            for (Path versionDir : versionDirs) {
                String dirName = versionDir.getFileName().toString();
                if (!dirName.contains("-") && dirName.contains(releaseCode)) {
                    Path systemModules = versionDir.resolve("system-modules");

                    if (Files.isRegularFile(systemModules)) {
                        Path modules = FileSystems.getFileSystem(URI.create("jrt:/")).getPath("modules");
                        List<String> lines = Files.readAllLines(systemModules, Charset.forName("UTF-8"));
                        for (String moduleName : lines) {
                            Path path = modules.resolve(moduleName);
                            if (Files.exists(path)) {
                                includedSysModuleNames.put(moduleName, path);
                            }
                        }
                    } else {
                        platformPath.add(versionDir);
                    }
                }
            }

            for (Map.Entry<String, Path> path : includedSysModuleNames.entrySet()) {
                fileManager.setLocationForModule(StandardLocation.SYSTEM_MODULES, path.getKey(), List.of(path.getValue()));
            }

            Path releaseModules = root.resolve("" + releaseCode + "-modules");
            if (Files.isDirectory(releaseModules)) {
                for (Path path : Files.list(releaseModules).collect(Collectors.toList())) {
                    String moduleName = path.getFileName().toString();
                    ArrayList<Path> paths = new ArrayList<>();
                    paths.add(path);
                    paths.addAll(platformPath);
                    includedSysModuleNames.put(moduleName, path);
                    fileManager.setLocationForModule(StandardLocation.SYSTEM_MODULES, moduleName, paths);
                }
            } else if (includedSysModuleNames.isEmpty()) {
                // Clear the platform classpath...
                fileManager.handleOption("--system", List.of("none").iterator());
                fileManager.setLocationFromPaths(StandardLocation.PLATFORM_CLASS_PATH, platformPath);
            }
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }

        optionSetter.accept(fileManager);
    }

    @Override
    public StandardJavaFileManager fileManager() {
        return fileManager;
    }

    private static FileSystem ctSymFileSystem;

    private static FileSystem getCtSymFileSystem() {
        if (ctSymFileSystem == null) {
            try {
                Path ctsym = Paths.get(System.getProperty("java.home"), "lib", "ct.sym");
                if (Files.isRegularFile(ctsym)) {
                    ctSymFileSystem = FileSystems.newFileSystem(ctsym, null);
                }
            } catch (IOException e) {
                throw new RuntimeException(e);
            }

        }
        return ctSymFileSystem;
    }

    public ModuleFinder moduleFinder() {
        return ModuleFinder.compose(
                new FilteringSystemModuleFinder(includedSysModuleNames),
                locationToFinder(fileManager, StandardLocation.CLASS_OUTPUT),
                locationToFinder(fileManager, StandardLocation.MODULE_PATH)
        );
    }
}
