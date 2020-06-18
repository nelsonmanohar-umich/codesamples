class Solution:
    '''
    Given an integer num, find the closest two integers in
    absolute difference whose product equals num + 1 or num + 2.
    Return the two integers in any order.

    Example 1:
    Input: num = 8
    Output: [3,3]
    Explanation: For num + 1 = 9, the closest divisors are 3 & 3, for num + 2 = 10, the closest divisors are 2 & 5, hence 3 & 3 is chosen.

    Example 2:
    Input: num = 123
    Output: [5,25]
    '''
    def closestDivisors(self, num: int) -> List[int]:
        # not speedy at all but a valid solution
        from math import floor
        n = num
        n1 = num + 1
        n2 = num + 2

        r = int(floor(sqrt(n2)))

        def valid(a,b):
            return int(a)*int(b) in (n1, n2)

        if n < 2:
            return [1, 2]

        f1 = r
        while f1 > 1:
            f2a, f2b = n1//f1, n2//f1
            if valid(f1, f2a) and valid(f1, f2b):
                if abs(f1-f2a) < abs(f1-f2b):
                    return [f1, f2a]
                return [f1, f2b]
            elif valid(f1, f2b):
                return [f1, f2b]
            elif valid(f1, f2a):
                return [f1, f2a]
            f1 -= 1

