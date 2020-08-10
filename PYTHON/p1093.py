class Solution:
    '''
    We sampled integers between 0 and 255, and stored the results in an array
    count:  count[k] is the number of integers we sampled equal to k.

    Return the minimum, maximum, mean, median, and mode of the sample
    respectively, as an array of floating point numbers.  The mode is
    guaranteed to be unique.
    '''
    def sampleStats(self, count: List[int]) -> List[float]:
        from collections import OrderedDict

        freqs = dict([(v, f) for v, f in enumerate(count) if f])
        vmin, vmax, vavg, vmed, vmod = 0, 0, 0, 0, 0
        vals = list(freqs.keys())
        vmin, vmax = vals[0], vals[-1]
        n = sum(count)
        mi = [n//2-1, n//2] if n % 2 == 0 else [n//2, n//2]
        vavg = sum([v * freqs[v] for v in freqs.keys()])/n

        i = 0
        vmod = vals[0]
        for v in vals:
            vmod = v if freqs[v] > freqs[vmod] else vmod
            lo, hi = i, i + freqs[v]
            if lo <= mi[0] <= hi: v1 = v
            if lo <= mi[1] <= hi: v2 = v
            i += freqs[v]
        vmed = (v1 + v2) / 2

        return [vmin, vmax, vavg, vmed, vmod]



