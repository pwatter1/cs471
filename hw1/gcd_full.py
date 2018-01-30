#! /usr/bin/env python
from __future__ import division
import sys

def gcdI(i, j):
	while i != j:
		if i > j:
			i -= j
		else:
			j -= i
	return i

def gcdR(i, j):
	if j == 0:
		return i
	else:
		return gcdR(j, i % j)

if len(sys.argv) == 3:
	try:
		a = int(sys.argv[1])
		b = int(sys.argv[2])
		print("%d" % gcdI(a,b))
		print("%d" % gcdR(a,b))

	except ValueError:
		print("failed to parse all arguments as integers.")
		exit(1)
else:
	print("Not enough numbers to get GCD")
	print("%s usage: [NUMBER] [NUMBER]" % sys.argv[0])


