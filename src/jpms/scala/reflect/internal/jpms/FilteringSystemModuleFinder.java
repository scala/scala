package scala.reflect.internal.jpms;

import java.io.IOException;
import java.lang.module.ModuleDescriptor;
import java.lang.module.ModuleFinder;
import java.lang.module.ModuleReference;
import java.nio.ByteBuffer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

/** Filters `ModuleFinder.ofSystem()` to return only the modules named in `included` */
class FilteringSystemModuleFinder implements ModuleFinder {

    private final HashMap<String, ModuleReference> includedModules;
    private final HashSet<ModuleReference> includedModulesSet;

    FilteringSystemModuleFinder(HashMap<String, Path> included) {
        includedModules = new HashMap<>();
        includedModulesSet = new LinkedHashSet<>();
        Set<ModuleReference> all = ModuleFinder.ofSystem().findAll();
        for (ModuleReference x : all) {
            String moduleName = x.descriptor().name();
            if (included.containsKey(moduleName)) {
                Path path = included.get(moduleName);
                Path sigFile = path.resolve("module-info.sig");
                if (Files.exists(sigFile)) {
                    ModuleDescriptor descriptor = readDescriptor(sigFile);
                    includedModules.put(moduleName, new FixedModuleReference(descriptor, path.toUri()));
                } else {
                    includedModules.put(moduleName, x);
                }
            }
        }
    }

    private ModuleDescriptor readDescriptor(Path sigFile) {
        try {
            return ModuleDescriptor.read(ByteBuffer.wrap(Files.readAllBytes(sigFile)));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public Optional<ModuleReference> find(String name) {
        ModuleReference value = includedModules.get(name);
        return Optional.ofNullable(value);
    }

    @Override
    public Set<ModuleReference> findAll() {
        return includedModulesSet;
    }
}
