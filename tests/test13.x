class Square {
	static var height = 10;
	static var width = 10;
	
	static area() {
		var a = height * width;
		return a;
	}

	static main() {
		return area();
	}
}

class Rectangle {
	static var s = new Square();
	static main() {
		return s.height;
	}
}