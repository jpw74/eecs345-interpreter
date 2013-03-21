var x = 0;
var y = 0;
var total = 0;

while (x <= y) {
	if (x == 2) {
		x = x + 2;
		continue;
	}
	total = x - 1;
	x = x + 2;
	y = y + 1;
}
return total;