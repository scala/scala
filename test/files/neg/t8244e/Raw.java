public abstract class Raw<T>{
        public Raw raw() { return new Raw<String>() { public String t() { return ""; } }; }
        public abstract T t();
}
