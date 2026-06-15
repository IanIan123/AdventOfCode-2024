// AI Generated
var contents = File.ReadAllText("data.txt");
var lines = contents.Split('\n');
int height = lines.Length;
int width = lines[0].Length;

(int r, int c) start = default, end = default;
var walls = new HashSet<(int, int)>();

for (int r = 0; r < height; r++)
    for (int c = 0; c < width; c++)
    {
        if (c >= lines[r].Length) continue;
        switch (lines[r][c])
        {
            case 'S': start = (r, c); break;
            case 'E': end   = (r, c); break;
            case '#': walls.Add((r, c)); break;
        }
    }

(int r, int c)[] directions = [(-1, 0), (0, 1), (1, 0), (0, -1)];

Dictionary<(int, int), int> BFS((int r, int c) origin)
{
    var dist = new Dictionary<(int, int), int> { [origin] = 0 };
    var queue = new Queue<(int r, int c)>();
    queue.Enqueue(origin);
    while (queue.Count > 0)
    {
        var pos = queue.Dequeue();
        int d = dist[pos];
        foreach (var (r, c) in directions)
        {
            var next = (pos.r + r, pos.c + c);
            if (!dist.ContainsKey(next) && !walls.Contains(next))
            {
                dist[next] = d + 1;
                queue.Enqueue(next);
            }
        }
    }
    return dist;
}

var fromStart = BFS(start);
var fromEnd   = BFS(end);
int noCheatCount = fromStart[end];
Console.WriteLine($"No-cheat count: {noCheatCount}");

void SolvePart(int maxDistance, string label)
{
    var allPoints = fromStart.Keys.ToList();
    int savings100Plus = 0;
    for (int i = 0; i < allPoints.Count; i++)
    {
        var a = allPoints[i];
        for (int j = i + 1; j < allPoints.Count; j++)
        {
            var b = allPoints[j];
            int manhattan = Math.Abs(a.Item1 - b.Item1) + Math.Abs(a.Item2 - b.Item2);
            if (manhattan > maxDistance) continue;
            if (noCheatCount - (fromStart[a] + manhattan + fromEnd[b]) >= 100) savings100Plus++;
        }
    }
    Console.WriteLine($"{label} Savings: {savings100Plus}");
}

SolvePart(2,  "Part 1");
SolvePart(20, "Part 2");