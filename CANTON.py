# Copyright (c) 2015 Linus Yang

from math import sqrt, floor

def main():
    inp = raw_input()
    for i in xrange(int(inp)):
        x = int(raw_input())
        n = int(floor(0.5 * (-1 + sqrt(1 + 8 * x))))
        idx = x - (1 + n) * n / 2
        sum = n + 2
        fromTop = True if sum % 2 == 1 else False
        if idx == 0:
            result = "%d/%d" % ((1, sum - 2) if fromTop else (sum - 2, 1))
        else:
            result = "%d/%d" % ((idx, sum - idx) if fromTop else (sum - idx, idx))
        print("TERM %d IS %s" % (x, result))

if __name__ == '__main__':
    main()
