var x = 0;
var y = 10;
main() {
	while (!(x >= y) || !(y > 25)) {
	  x = x + 2;
	  y = y + 1;
	}
	return x;
}