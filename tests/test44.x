add (x, y, z) {
	var w;
	w = y + x + z;
	var q;
	q = divide (w, 2);
	return q;
}

divide (x, y) {
	return x / y;
}

main () {
	add (4, 5, 3);
}