from cytoolz.itertoolz import iterate

def squareroot(N, eps):
    def _improve(a):
        return (a + N/a) / 2.0

    def _within(gen):
        prev = gen.next()
        cur = gen.next()
        while abs(cur - prev) > eps:
            prev = cur
            cur = gen.next()
        return cur

    return _within(iterate(_improve, 1))
