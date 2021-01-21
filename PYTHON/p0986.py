class Solution:
    '''
    You are given two lists of closed intervals, firstList and secondList,
    where firstList[i] = [starti, endi] and secondList[j] = [startj, endj].
    Each list of intervals is pairwise disjoint and in sorted order.
    Return the intersection of these two interval lists.
    '''
    def intervalIntersection(self, A: List[List[int]], B: List[List[int]]) -> List[List[int]]:

        n, m = len(A), len(B)
        if n == 0:
            return []
        if m == 0:
            return []
        if A == B:
            return A

        out = []
        A, B = A[::-1], B[::-1]

        a, b = A.pop(), B.pop()
        while a and b:
            if a > b:
                out.append(b)
                b = B.pop() if B else None
            elif b > a:
                out.append(a)
                a = A.pop() if A else None
            else:
                out.append(a)
                out.append(b)
                a = A.pop() if A else None
                b = B.pop() if B else None

        if a: out.append(a)
        if b: out.append(b)
        if A: out.extend(A[::-1])
        if B: out.extend(B[::-1])

        out = out[::-1]
        inter = []
        c, n = None, None
        c = out.pop()
        c_min, c_max = c
        while out:
            n = out.pop()
            c_min, c_max = c
            n_lo, n_hi = n

            if n == c:
                i = [n_lo, n_hi]
                inter.append(i)
                continue

            if n_lo < c_max:
                if c_max == n_hi:
                    i = [n_lo, n_hi]
                elif c_max < n_hi:
                    i = [n_lo, c_max]
                    c = [n_lo, n_hi]
                else:
                    i = [n_lo, n_hi]
                inter.append(i)

            if n_lo == c_max:
                i = [n_lo, n_lo]
                inter.append(i)
                c = n

            if n_lo > c_max:
                i = []
                c = n

        return inter






