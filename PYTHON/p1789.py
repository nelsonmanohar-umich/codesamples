class Solution:
    # https://leetcode.com/problems/count-number-of-homogenous-substrings/
    def countHomogenous(self, s: str) -> int:
        letters = string.ascii_lowercase
        n = len(s)
        if n <= 1: return n

        MOD = 10**9 + 7

        def sol1():
            out = []
            c0, cnt = s[0], 1
            tot = 0
            for c1 in s[1:]:
                if c1 == c0:
                    cnt += 1
                else:
                    out.append((c0, cnt))
                    tot += (cnt*cnt + cnt) // 2
                    c0, cnt = c1, 1

            if c0 == c1:
                out.append((c1, cnt))
                tot += (cnt*cnt + cnt) // 2
            else:
                out.append((c1, 1))
                tot += 1
            return tot % MOD

        def sol2():
            out = []
            c0, cnt = s[0], 1
            for c1 in s[1:]:
                if c1 == c0:
                    cnt += 1
                else:
                    out.append((c0, cnt))
                    c0, cnt = c1, 1

            if c0 == c1:
                out.append((c1, cnt))
            else:
                out.append((c1, 1))

            counts = Counter(s)
            total = 0
            for i, l in enumerate(letters):
                options = [cnt for c, cnt in out if c == l]
                for m in options:
                    total += (m*m + m) // 2
            return total % MOD

        return sol1()


