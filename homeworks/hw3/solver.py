import sys
import math

# 2(100T + 10W + O) = 1000F + 100O + 10U + R

# 2O = 10x_1 + R
# 2W = 10x_2 + U - x_1
# 2T = 10F + O - x_2

for t in range(1, 10):
	for w in range(0, 10):
		for o in range(0, 10):
			for u in range(0, 10):
				for f in range(1, 3):
					for r in range(0, 10):
						if 2 * (100 * t + 10 * w + o) == 1000 * f + 100 * o + 10 * u + r:
							# print "satisfies the single equation:"
							# print "TWO: " + str(t) + str(w) + str(o)
							# print "FOUR: " + str(f) + str(o) + str(u) + str(r)
							# print ""
							good = False
							for x1 in range(0, 2):
								for x2 in range(0, 2):
									if (2 * o == 10 * x1 + r and 2 * w == 10 * x2 + u - x1 and 2 * t == 10 * f + o - x2):
										good = True
							if not good:
								print "satisfies the single equation but not equation set:"
								print "TWO: " + str(t) + str(w) + str(o)
								print "FOUR: " + str(f) + str(o) + str(u) + str(r)
								print x1
								print x2
								print ""

for f in range(1, 10):
	for o in range(0, 10):
		for r in range(0, 10):
			for t in range(0, 10):
				for y in range(0, 10):
					for e in range(0, 10):
						for n in range(0, 10):
							for s in range(0, 10):
								for i in range(0, 10):
									for x in range(0, 10):
										if 2 * (100 * t + 10 * e + n) + 10000 * f + 1000 * o + 100 * r + 10 * t + y == 10000 * s + 1000 * i + 100 * x + 10 * t + y:
											# print "satisfies the single equation:"
											# print "TWO: " + str(t) + str(w) + str(o)
											# print "FOUR: " + str(f) + str(o) + str(u) + str(r)
											# print ""
											good = False
											for x1 in range(0, 3):
												for x2 in range(0, 3):
													for x3 in range(0, 3):
														for x4 in range(0, 3):
															if (2 * n + y == 10 * x1 + y and 2 * e + t == 10 * x2 + t - x1 and 2 * t + r == 10 * x3 + x - x2 and o == 10 * x4 + i - x3 and f == s - x4):
																good = True
											if not good:
												print "satisfies the single equation but not equation set:"
												print "forty: " + str(f) + str(o) + str(r) + str(t) + str(y)
												print "ten: " + str(t) + str(e) + str(n)
												print "sixty: " + str(s) + str(i) + str(x) + str(t) + str(y)
