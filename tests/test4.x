class A {
  static var x = 10;
  static var y = 20;

  static add(a, b) {
    return a + b;
  }

  static main() {
    return A.add(A.x, y);
  }
}
