var x = 0;
var y = 0;
var total = 0;

while (x <= 2) {
     	while (y <= 2) {
		if (y == 1) {
			y = y + 1;
			continue; 
		}
		total = total + (y + x);
		y = y + 1; 
	}
total = total + x;	
x = x + 1;
}
return total;