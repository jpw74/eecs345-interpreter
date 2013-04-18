var x = 1;
main() {
	while (true) {
	  x = x + 1;
	  if (x > 10 && x % 2 == 0)
	   break;
	}
	return x;
}