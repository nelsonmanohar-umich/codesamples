class Solution:
    """
    An image is represented by a binary matrix with 0 as a white pixel
    and 1 as a black pixel. The black pixels are connected, i.e., there
    is only one black region. Pixels are connected horizontally and
    vertically. Given the location (x, y) of one of the black pixels,
    return the area of the smallest (axis-aligned) rectangle that encloses
    all black pixels.
    """
    def minArea(self, image: List[List[str]], x: int, y: int) -> int:
        I = [[int(c) for c in s] for s in image]

        n = len(I)
        m = len(I[0]) if n else 0

        if n == 0: return 0
        if m == 0: return 0
        if n == 1: return sum(I[0])
        if m == 1: return sum([I[i][0] for i in range(n)])

        It = list(zip(*I))

        hsums = [1 if sum(I[i]) else 0 for i in range(n)]
        vsums = [1 if sum(It[i]) else 0 for i in range(m)]
        return sum(hsums) * sum(vsums)


