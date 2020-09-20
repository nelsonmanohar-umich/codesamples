class Solution:
    '''
    Given an array arr of 4 digits, find the latest 24-hour time that can be made using
    each digit exactly once.

    24-hour times are formatted as "HH:MM", where HH is between 00 and 23, and MM is
    between 00 and 59. The earliest 24-hour time is 00:00, and the latest is 23:59.

    Return the latest 24-hour time in "HH:MM" format.  If no valid time can be made,
    return an empty string.
    '''
    def largestTimeFromDigits(self, arr: List[int]) -> str:
        INV = "9999"
        digits = "".join([str(x) for x in arr])

        @lru_cache
        def str2time(s):
            h, m = s[:2], s[2:]
            t = int(h) * 60 + int(m)
            if int(h) >=24 or int(m) >= 60: return -1
            return t

        self.maxt = INV

        def dfs(digits, path="", visited={}):
            key = (path, digits)
            if key in visited: return
            visited[key] = True

            if len(path) == 4:
                t = str2time(path)
                if t > str2time(self.maxt): self.maxt = path
                return

            for i in range(len(digits)):
                what = digits[0:i] + digits[i+1:]
                dfs(what, path+digits[i], visited)
            return

        path, visited = "", {}
        dfs(digits, path, visited)

        return "%s:%s" % (self.maxt[:2], self.maxt[2:]) if self.maxt != INV else ""







