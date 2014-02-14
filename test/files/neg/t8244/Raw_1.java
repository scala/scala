public abstract class Raw_1<T>{
        public Raw_1 raw() { return new Raw_1<String>() { public String t() { return ""; } }; }
        public abstract T t();
}
