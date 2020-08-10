class Solution:
    '''
    Given a list of daily temperatures T, return a list such that, for each day
    in the input, tells you how many days you would have to wait until a warmer
    temperature. If there is no future day for which this is possible, put 0
    instead.

    For example, given the list of temperatures T = [73, 74, 75, 71, 69, 72, 76, 73],
    your output should be [1, 1, 4, 2, 1, 1, 0, 0].

    Note: The length of temperatures will be in the range [1, 30000]. Each temperature
    will be an integer in the range [30, 100].
    '''
    def dailyTemperatures(self, T: List[int]) -> List[int]:
        # not a good performance version
        n = len(T)
        dist = [0] * n
        if n == 0: return []
        if n == 1: return [0]
        tmin, tmax, imin, imax = T[-1], T[-1], n-1, n-1

        v2i = {}
        for i in range(n)[::-1]:
            if T[i] > tmax:
                tmax, imax = T[i], i
                dist[i] = 0
                v2i[T[i]] = i

            if T[i] < tmin:
                tmin, imin = T[i], i
                dist[i] = 1
                v2i[T[i]] = i

            if tmin <= T[i] <= tmax:
                tkeys = [t for t in v2i.keys() if t > T[i]]
                jmin = n
                for t in tkeys:
                    j = v2i[t]
                    if j < jmin:
                        jmin = j
                if jmin != n:
                    dist[i] = (jmin - i)
                v2i[T[i]] = i

        return dist

