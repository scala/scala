package scala.reflect.internal.jpms;

import java.lang.module.ModuleDescriptor;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;

/**
 * Internal representation of a module-info.java file, for use with Java reflection in the compiler's implementation
 * of module access checking.
 *
 * See https://docs.oracle.com/javase/specs/jls/se9/html/jls-7.html#jls-ModuleDeclaration
 *
 * TODO: imports
 */
public class JpmsModuleDescriptor {
    private final String name;
    private final List<RequireDirective> requires;

    // TODO JPMS flesh out with other directives
    public JpmsModuleDescriptor(String name, List<RequireDirective> requires) {
        this.name = name;
        this.requires = requires;
    }

    public static class RequireDirective {
        private final String name;
        private final boolean transitive;
        private final boolean isStatic;

        public RequireDirective(String name, boolean transitive, boolean isStatic) {
            this.name = name;
            this.transitive = transitive;
            this.isStatic = isStatic;
        }
        public void buildTo(ModuleDescriptor.Builder builder) {
            Set<ModuleDescriptor.Requires.Modifier> mods = EnumSet.noneOf(ModuleDescriptor.Requires.Modifier.class);
            if (isStatic) mods.add(ModuleDescriptor.Requires.Modifier.STATIC);
            if (transitive) mods.add(ModuleDescriptor.Requires.Modifier.TRANSITIVE);
            builder.requires(mods, name);
        }
    }

    public ModuleDescriptor create() {
        ModuleDescriptor.Builder builder = ModuleDescriptor.newModule(name);
        for (RequireDirective require: requires) {
            builder.requires(require.name);
        }
        return builder.build();
    }
}
