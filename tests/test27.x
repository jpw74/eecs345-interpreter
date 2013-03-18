var x = 6;
{
    var x = 5;
    var y = 7;
    {
        var z = 5;
        y = 8;
        x = 4;
        return x + y + z;
    }
}
