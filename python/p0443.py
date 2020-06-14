class Solution:
    '''
    Given an array of characters, compress it in-place.
    The length after compression must always be smaller than or equal
    to the original array.
    Every element of the array should be a character (not int) of length 1.
    After you are done modifying the input array in-place, return the
    new length of the array.
    Follow up: Could you solve it using only O(1) extra space?
    '''
    def compress(self, chars: List[str]) -> int:
        n = len(chars)
        if n == 0:
            return 0
        if n == 1:
            return 2

        rle = []
        old_c, cnt = chars[0], 1
        i = 1
        while i < n:
            c = chars[i]
            if c == old_c:
                old_c, cnt = old_c, cnt+1
            else:
                rle.append([old_c, str(cnt)])
                old_c, cnt = c, 1
            i = i + 1
        rle.append([old_c, str(cnt)])

        single_rle = []
        for i, c in enumerate(rle):
            single_rle.append(rle[i][0])
            if int(rle[i][1]) > 1:
                single_rle.extend(list(rle[i][1]))

        for i, c in enumerate(single_rle):
            chars[i] = single_rle[i]

        if len(single_rle) < n:
            chars[len(single_rle):] = []

        return len(chars)
