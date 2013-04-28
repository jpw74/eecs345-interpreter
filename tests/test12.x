class A {
  static var x = 10;
  static var y = 20;

  static add(a, b) {
    return a + b;
  }

  static add(a,b,c) {
    return a + b + c;
  }
}

class B extends A {
  static var x = 2;
  static var y = 30;

  static add(a,b) {
    return a*b;
  }

  static main() {
    return add(x,y) + add(x,x,x);
  }
}
