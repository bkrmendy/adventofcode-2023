import z3

def find_intersection_ray(hailstones):
    solver = z3.Solver()
    
    positions = z3.IntVector("p", 3)
    velocities = z3.IntVector("v", 3)
    intersection_times = z3.IntVector("t", len(hailstones))
    
    solver.add(z3.And([t >= 0 for t in intersection_times]))
    
    for t, (p, v) in zip(intersection_times, hailstones):
        for j in range(3):
            solver.add(positions[j] + velocities[j] * t == p[j] + v[j] * t)
    
    solver.check()
    
    model = solver.model()
    
    return [model[positions[i]].as_long() for i in range(3)]

res = find_intersection_ray([
    ((359781776524153, 312705660279075, 236728636905923), (-44, -125, 18)),
    ((276481733510955, 270867065789660, 273768862611813), (35, 20, 33)),
    ((189537654420103, 292422605212995, 333617095281945), (102, -15, -14)),
])

print(sum(res))