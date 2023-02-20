class Test {
    private void method1() {
        log.info("method1");
    }

    public int method2(int i) {
        return i + foo();
    }

    public Editor method(Context c0) {
        return c0.getBean("Editor");
    }
}
