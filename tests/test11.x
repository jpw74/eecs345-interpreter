class A {
  static var x = 10;
  static var y = 20;

  static add(a, b) {
    return a + b;
  }

  static add(a,b,c) {
    return a + b + c;
  }

  static main() {
    return A.add(x, y) + A.add(x, y, y) * 10;
  }
}
