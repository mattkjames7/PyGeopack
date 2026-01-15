import PyGeopack as gp


field = gp.ModelField(5, 0, 0, 20200101, 12.0, Model='T96')
field = [x[0] for x in field]
print("Magnetic field at (5,0,0) in GSM at noon on 2020-01-01 (T96): ", field)

trace = gp.TraceField(5, 0, 0, 20200101, 12.0, Model='T96')
nsteps = trace.nstep[0]
print("Number of steps to trace from (5,0,0) in GSM at noon on 2020-01-01 (T96): ", nsteps)

print("Test run complete.")
