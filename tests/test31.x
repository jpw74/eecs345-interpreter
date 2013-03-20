var x = 0;
var total = 0;
while (x <= 5) {
    if (x == 4) {
        x = x + 1;
        continue;
    }
    total = total + x;
    x = x + 1;
}
return total;
