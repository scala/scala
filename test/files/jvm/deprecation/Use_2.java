class Use_2 {
    public int test() {
        Test u = new Test();
        Test.Inner a = u.new Inner();
        int i = a.f();
        int j = a.g();
        a.setG(5);
        return i + j;
    }
}