class Solution:
    '''
    Given a rectangular cake with height h and width w, and two arrays
    of integers horizontalCuts and verticalCuts where horizontalCuts[i]
    is the distance from the top of the rectangular cake to the ith
    horizontal cut and similarly, verticalCuts[j] is the distance from
    the left of the rectangular cake to the jth vertical cut.

    Return the maximum area of a piece of cake after you cut at each
    horizontal and vertical position provided in the arrays horizontalCuts
    and verticalCuts. Since the answer can be a huge number, return this
    modulo 10^9 + 7.
    '''
    def maxArea(self, h: int, w: int, horizontalCuts: List[int], verticalCuts: List[int]) -> int:
        H, V = horizontalCuts, verticalCuts
        H = sorted(set([0] + H + [h]))
        V = sorted(set([0] + V + [w]))
        n, m = len(H), len(V)
        self.maxsize = 0

        def sol0():
            A = [[(H[i]-H[i-1])*(V[j]-V[j-1]) for j in range(1,m)] for i in range(1,n)]
            return max([max(AA) for AA in A])

        def sol1():
            for i in range(1,n):
                for j in range(1,m):
                    h1, v1 = H[i],V[j]
                    h0, v0 = H[i-1], V[j-1]
                    A = (h1-h0) * (v1-v0)
                    if A > self.maxsize:
                        self.maxsize = A
            return self.maxsize

        def sol2():
            Hgaps = [(b-a) for a,b in zip(H, H[1:])]
            Vgaps = [(b-a) for a,b in zip(V, V[1:])]
            Hgaps.sort(reverse=True)
            Vgaps.sort(reverse=True)
            self.maxsize = 0
            return (Hgaps[0] * Vgaps[0]) % 1000000007

        return sol2()
