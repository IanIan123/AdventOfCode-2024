// AI generated

var lines = File.ReadAllLines("data.txt").Where(l => l.Length > 0).ToArray();
var pairs = lines.Select(l => l.Split(',').Select(int.Parse).ToArray())
                  .Select(p => (X: p[0], Y: p[1])).ToArray();

int cols = pairs.Max(p => p.X) + 1;
int rows = pairs.Max(p => p.Y) + 1;
var start = (X: 0, Y: 0);
var end = (X: cols - 1, Y: rows - 1);

int Bfs(HashSet<(int,int)> obstacles)
{
    var visited = new HashSet<(int,int)> { start };
    var queue = new Queue<((int X,int Y) pos, int dist)>();
    queue.Enqueue((start, 0));

    while (queue.Count > 0)
    {
        var (pos, dist) = queue.Dequeue();
        if (pos == end) return dist;

        var next = new[] { (-1,0), (1,0), (0,-1), (0,1) }
            .Select(d => (X: pos.X + d.Item1, Y: pos.Y + d.Item2))
            .Where(p => p.X >= 0 && p.Y >= 0 && p.X < cols && p.Y < rows
                     && !obstacles.Contains(p) && visited.Add(p));

        foreach (var p in next) queue.Enqueue((p, dist + 1));
    }
    return -1;
}

int limit = pairs.Length >= 1024 ? 1024 : 12;

// Part 1
Console.WriteLine($"Path length: {Bfs([.. pairs.Take(limit)])}");

// Part 2
var blocker = Enumerable.Range(limit + 1, pairs.Length - limit)
    .Select(i => (i, blocked: Bfs([.. pairs.Take(i)]) == -1))
    .First(r => r.blocked);

var b = pairs[blocker.i - 1];
Console.WriteLine($"Obstacle: {b.X},{b.Y}");