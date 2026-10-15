package p;

// Typing `B.E` while completing `C` completes `B`, whose parents include `C`
public sealed interface C permits B, B.E {}
