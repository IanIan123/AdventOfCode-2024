// AI generated
var lines = File.ReadAllLines("data.txt").Where(l => l.Length > 0).ToArray();
var pairs = lines.Select(l => l.Split(',').Select(int.Parse).ToArray())
    .Select(p => (X: p[0], Y: p[1])).ToArray();

int cols = pairs.Max(p => p.X) + 1;
int rows = pairs.Max(p => p.Y) + 1;
var start = (X: 0, Y: 0);
var end = (X: cols - 1, Y: rows - 1);

(int dist, List<(int X,int Y)>? path) Bfs(HashSet<(int,int)> obstacles)
{
    var visited = new HashSet<(int,int)> { start };
    var parent = new Dictionary<(int,int),(int,int)>();
    var queue = new Queue<((int X,int Y) pos, int dist)>();
    queue.Enqueue((start, 0));

    while (queue.Count > 0)
    {
        var (pos, dist) = queue.Dequeue();
        if (pos == end)
        {
            var path = new List<(int,int)> { pos };
            while (parent.TryGetValue(path[^1], out var p)) path.Add(p);
            path.Reverse();
            return (dist, path);
        }

        var next = new[] { (-1,0), (1,0), (0,-1), (0,1) }
            .Select(d => (X: pos.X + d.Item1, Y: pos.Y + d.Item2))
            .Where(p => p.X >= 0 && p.Y >= 0 && p.X < cols && p.Y < rows
                     && !obstacles.Contains(p) && visited.Add(p));

        foreach (var p in next)
        {
            parent[p] = pos;
            queue.Enqueue((p, dist + 1));
        }
    }
    return (-1, null);
}

void RenderGrid(HashSet<(int,int)> obstacles, List<(int,int)>? path)
{
    var pathSet = path?.ToHashSet() ?? new HashSet<(int,int)>();
    Console.WriteLine("\n=== Grid ===");
    for (int y = 0; y < rows; y++)
    {
        for (int x = 0; x < cols; x++)
        {
            var p = (x, y);
            Console.Write(obstacles.Contains(p) ? '#' : pathSet.Contains(p) ? 'O' : '.');
        }
        Console.WriteLine();
    }
}

int limit = pairs.Length >= 1024 ? 1024 : 12;

// Part 1
var part1Obstacles = pairs.Take(limit).ToHashSet();
var (dist, path) = Bfs(part1Obstacles);
RenderGrid(part1Obstacles, path);
Console.WriteLine($"Path length: {dist}");

// Part 2 - binary search over obstacle count
int lo = limit + 1, hi = pairs.Length - 1;
while (lo < hi)
{
    int mid = (lo + hi) / 2;
    if (Bfs(pairs.Take(mid).ToHashSet()).dist == -1) hi = mid;
    else lo = mid + 1;
}

var b = pairs[lo - 1];
Console.WriteLine($"Obstacle: {b.X},{b.Y}");