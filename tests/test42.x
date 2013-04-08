var a = 1;
var b = 2;
swap(&x, &y) {
    var temp = x;
    x = y;
    y = temp;
}

main() {
    swap(a, b);
    return b;
}
