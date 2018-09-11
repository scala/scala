package scala.reflect.internal.jpms;

import java.io.IOException;
import java.io.InputStream;
import java.lang.module.ModuleReader;
import java.net.URI;
import java.nio.ByteBuffer;
import java.util.Optional;
import java.util.stream.Stream;

class NoopModuleReader implements ModuleReader {
    static final ModuleReader INSTANCE = new NoopModuleReader();

    @Override
    public Optional<URI> find(String name) throws IOException {
        return Optional.empty();
    }

    @Override
    public Optional<InputStream> open(String name) throws IOException {
        return Optional.empty();
    }

    @Override
    public Optional<ByteBuffer> read(String name) throws IOException {
        return Optional.empty();
    }

    @Override
    public void release(ByteBuffer bb) {
    }

    @Override
    public Stream<String> list() throws IOException {
        return Stream.empty();
    }

    @Override
    public void close() throws IOException {
    }
}
