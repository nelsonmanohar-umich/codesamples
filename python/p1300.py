class Solution:
    '''
    Given an integer array arr and a target value target, return
    the integer value such that when we change all the integers
    larger than value in the given array to be equal to value,
    the sum of the array gets as close as possible (in absolute
    difference) to target.

    In case of a tie, return the minimum such integer.
    Notice that the answer is not neccesarilly a number from arr.

    Example 1:
    Input: arr = [4,9,3], target = 10
    Output: 3
    Explanation: When using 3 arr converts to [3, 3, 3] which sums 9 and that's the optimal answer.
    '''
    def findBestValue(self, arr: List[int], target: int) -> int:
        n = len(arr)
        #tot = sum(arr)
        avg = target/n

        pivot_a = int(ceil(avg))
        a_surplus = [pivot_a-x for x in arr if x < pivot_a]
        amortized_surplus = sum(a_surplus)//(n-len(a_surplus))
        pivot_a = pivot_a + amortized_surplus
        a = sum([x if x<=pivot_a else pivot_a for x in arr])

        pivot_b = pivot_a + 1
        b = sum([x if x<=pivot_b else pivot_b for x in arr])

        pivot_c = pivot_a - 1
        c = sum([x if x<=pivot_c else pivot_c for x in arr])

        vals = sorted([(abs(target-x), y) for x,y in zip((a,b,c), (pivot_a, pivot_b, pivot_c))])
        val = vals[0][1]
        return val if val <= max(arr) else max(arr)

