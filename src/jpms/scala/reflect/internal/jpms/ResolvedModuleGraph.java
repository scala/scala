package scala.reflect.internal.jpms;

import javax.tools.JavaFileManager;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.StandardLocation;
import java.io.IOException;
import java.lang.module.Configuration;
import java.lang.module.ModuleDescriptor;
import java.lang.module.ResolvedModule;
import java.nio.file.Path;
import java.util.*;

import static scala.reflect.internal.jpms.JpmsClasspathSupport.UNNAMED_MODULE_NAME;

public class ResolvedModuleGraph {
    private String currentModule;
    private final Configuration configuration;
    private StandardJavaFileManager fileManager;
    private Map<String, Set<JavaFileManager.Location>> modulePatchLocations = new HashMap<>();
    public static final Set<String> AUTOMATIC_MODULE_EXPORT_ALL = new HashSet<>();

    public ResolvedModuleGraph(String currentModule, Configuration configuration, StandardJavaFileManager fileManager) throws IOException {
        this.currentModule = currentModule;
        this.configuration = configuration;
        this.fileManager = fileManager;

        if (fileManager.hasLocation(StandardLocation.PATCH_MODULE_PATH)) {
            Iterable<Set<JavaFileManager.Location>> sets = fileManager.listLocationsForModules(StandardLocation.PATCH_MODULE_PATH);
            for (Set<JavaFileManager.Location> set: sets) {
                for (JavaFileManager.Location location: set) {
                    String moduleName = fileManager.inferModuleName(location);
                    Set<JavaFileManager.Location> locations = modulePatchLocations.computeIfAbsent(moduleName, (key) -> new HashSet<>());
                    locations.add(location);
                }
            }
        }
    }

    public Map<String, Set<String>> accessibleModulePackages(String siteModuleName) {
        Set<ResolvedModule> readModules;
        String finalSiteModuleName = siteModuleName;
        if (siteModuleName.equals("")) {
            readModules = configuration.modules();
            finalSiteModuleName = UNNAMED_MODULE_NAME;
        } else {
            readModules = configuration.findModule(siteModuleName).map(ResolvedModule::reads).orElse(Collections.emptySet());
        }

        HashMap<String, Set<String>> result = new HashMap<>();
        for (ResolvedModule readModule: readModules) {

            ModuleDescriptor descriptor = readModule.reference().descriptor();
            if (descriptor.isAutomatic()) {
                result.put(readModule.name(), AUTOMATIC_MODULE_EXPORT_ALL);
            } else {
                HashSet<String> packages = new HashSet<>();
                result.put(readModule.name(), packages);
                for (ModuleDescriptor.Exports exports: descriptor.exports()) {
                    if (!exports.isQualified() || exports.targets().contains(finalSiteModuleName)) {
                        packages.add(exports.source());
                    }
                }
            }
        }
        return result;
    }

    public String moduleForSourceFile(Path path) throws IOException {
        String result = currentModule;
        if (path == null) return result;

        if (fileManager.hasLocation(StandardLocation.PATCH_MODULE_PATH)) {
            JavaFileObject sourceFile = fileManager.getJavaFileObjects(path).iterator().next();
            Iterable<Set<JavaFileManager.Location>> sets = fileManager.listLocationsForModules(StandardLocation.PATCH_MODULE_PATH);
            for (Set<JavaFileManager.Location> set: sets) {
                for (JavaFileManager.Location location: set) {
                    if (fileManager.contains(location, sourceFile)) {
                        return fileManager.inferModuleName(location);
                    }
                }
            }
        }
        return result;
    }
}
