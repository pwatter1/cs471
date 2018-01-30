import sys

def ack(m, n):
    if m == 0:
        return n + 1
    elif n == 0:
        return ack(m - 1, 1)
    else:
        return ack(m - 1, ack(m, n - 1))

if len(sys.argv) == 3:
	try:
		a = int(sys.argv[1])
		b = int(sys.argv[2])
		print("%d" % ack(a,b))

	except ValueError:
		print("failed to parse all arguments as integers.")
		exit(1)
else:
	print("Not enough numbers to get ackermann")
	print("%s usage: [NUMBER] [NUMBER]" % sys.argv[0])


