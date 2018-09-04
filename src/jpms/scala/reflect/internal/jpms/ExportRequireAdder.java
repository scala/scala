package scala.reflect.internal.jpms;

import java.lang.module.ModuleDescriptor;
import java.util.*;

public class ExportRequireAdder {
    public Iterable<ModuleDescriptor.Exports> addExports(String moduleName) { return Collections.emptyList(); };
    public  Iterable<String> addReads(String moduleName) { return Collections.emptyList(); };;

    public ModuleDescriptor patch(ModuleDescriptor d) {
        ModuleDescriptor.Builder builder = ModuleDescriptor.newModule(d.name(), d.modifiers());

        addPatchedExports(d, builder);
        if (!d.isAutomatic())
            addPatchedRequires(d, builder);
        if (!d.isAutomatic() && !d.isOpen())
            d.opens().forEach(builder::opens);
        d.mainClass().ifPresent(builder::mainClass);
        d.provides().forEach(builder::provides);
        builder.packages(d.packages());
        d.rawVersion().ifPresent(builder::version);
        ModuleDescriptor build = builder.build();
        if (build.equals(d)) return d;
        else return build;
    }

    protected static Iterable<ModuleDescriptor.Exports> mkExport(String source, String target) {
        return ModuleDescriptor.newModule("dummy").exports(Set.of(), source, Collections.singleton(target)).build().exports();
    }


    private void addPatchedRequires(ModuleDescriptor d, ModuleDescriptor.Builder builder) {
        Map<String, ModuleDescriptor.Requires> newRequires = new LinkedHashMap<>();
        d.requires().forEach(x -> {newRequires.put(x.name(), x); builder.requires(x);});
        addReads(d.name()).forEach(x -> {if (!newRequires.containsKey(x)) builder.requires(x);});
    }

    private void addPatchedExports(ModuleDescriptor d, ModuleDescriptor.Builder builder) {
        Map<String, ModuleDescriptor.Exports> newExportsMap = new LinkedHashMap<>();
        for (ModuleDescriptor.Exports x : d.exports()) {
            newExportsMap.put(x.source(), x);
        }
        for (ModuleDescriptor.Exports x : addExports(d.name())) {
            if (newExportsMap.containsKey(x.source())) {
                ModuleDescriptor.Exports existing = newExportsMap.get(x.source());
                HashSet<String> newTargets = new HashSet<>(existing.targets());
                newTargets.addAll(x.targets());
                ModuleDescriptor.Exports newExports = ModuleDescriptor.newModule("dummy").exports(existing.modifiers(), existing.source(), newTargets).build().exports().iterator().next();
                newExportsMap.put(x.source(), newExports);
            } else {
                newExportsMap.put(x.source(), x);
            }
        }
        for (ModuleDescriptor.Exports exports : newExportsMap.values()) {
            builder.exports(exports);
        }
    }
}
