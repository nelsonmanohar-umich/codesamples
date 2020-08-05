class Solution:
    '''
    Given a list of 24-hour clock time points in "Hour:Minutes" format, find
    the minimum minutes difference between any two time points in the list.
    Example 1:
    Input: ["23:59","00:00"]
    Output: 1
    '''
    def findMinDifference(self, timePoints: List[str]) -> int:
        tmax = 24*60

        def to_mins(tval):
            hh, mm = tval.split(':')
            hh, mm = int(hh), int(mm)
            return hh*60 + mm

        def d212(t):
            return min(tmax-t, t)

        def tdist(t1, t2):
            if t1 == t2: return 0
            t1, t2 = min(t1, t2), max(t1, t2)
            a, b = d212(t1), d212(t2)
            return min(a+b, t2-t1)

        if len(set(timePoints)) != len(timePoints): return 0

        times = [to_mins(tval) for tval in timePoints]

        if len(times) > 8:
            times.sort()
            t0 = times[0]
            tmin = tmax
            for t1 in times[1:]:
                if t1-t0 < tmin: tmin = t1-t0
                t0 = t1
            return min(tmin,
                       tdist(times[0], times[-1]))

        n = len(times)
        tmin = tmax
        for i in range(n-1):
            t1 = times[i]
            for j in range(i+1, n):
                t2 = times[j]
                tval = tdist(t1, t2)
                if tval < tmin:
                    tmin = tval
        return tmin


