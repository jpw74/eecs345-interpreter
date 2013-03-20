var x = 0;
var total = 0;
while (x <= 6) {
    if (x == 5)
	break;
    total = total + x;
    x = x + 1;
}
return total;