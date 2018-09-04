package scala.reflect.internal.jpms;

import java.io.IOException;
import java.lang.module.ModuleDescriptor;
import java.lang.module.ModuleReader;
import java.lang.module.ModuleReference;
import java.net.URI;

final class FixedModuleReference extends ModuleReference {
    public FixedModuleReference(ModuleDescriptor descriptor, URI location) {
        super(descriptor, location);
    }

    @Override
    public ModuleReader open() throws IOException {
        return NoopModuleReader.INSTANCE;
    }

}
