// AI generated
var cache = new Dictionary<(long, int), long>();

List<long> Transform(long num)
{
    if (num == 0) return [1];
    var s = num.ToString();
    if (s.Length % 2 == 0)
    {
        int half = s.Length / 2;
        return [long.Parse(s[..half]), long.Parse(s[half..])];
    }
    return [num * 2024];
}

long Iterate(long num, int height)
{
    var key = (num, height);
    if (cache.TryGetValue(key, out long cached)) return cached;

    var next = Transform(num);
    long result = height == 1
        ? next.Count
        : next.Sum(n => Iterate(n, height - 1));

    return cache[key] = result;
}

var numbers = File.ReadAllText("data.txt")
    .Split(' ', StringSplitOptions.RemoveEmptyEntries)
    .Select(long.Parse)
    .ToList();

var start = DateTime.Now;
long total = numbers.Sum(n => Iterate(n, 75));
var elapsed = DateTime.Now - start;

Console.WriteLine($"Count: {total}");
Console.WriteLine($"Time: {elapsed.TotalMilliseconds:F1}ms");