package scala.reflect.internal.jpms;

import java.lang.module.ModuleDescriptor;
import java.lang.module.ModuleFinder;
import java.lang.module.ModuleReference;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

public final class FixedModuleFinder implements ModuleFinder {
    private List<ModuleReference> modules;

    public FixedModuleFinder(List<ModuleReference> modules) {
        this.modules = modules;
    }

    public static FixedModuleFinder newModuleFinder(List<ModuleDescriptor> modules) {
        List<ModuleReference> refs = modules.stream().map(x -> new FixedModuleReference(x, null)).collect(Collectors.toList());
        return new FixedModuleFinder(refs);
    }

    @Override
    public Optional<ModuleReference> find(String name) {
        return modules.stream().filter(x -> x.descriptor().name().equals(name)).findFirst();
    }

    @Override
    public Set<ModuleReference> findAll() {
        return new HashSet<>(modules);
    }
}
