class Solution:
    """
    Given an array nums containing n + 1 integers where each integer is between 1
    and n (inclusive), prove that at least one duplicate number must exist. Assume
    that there is only one duplicate number, find the duplicate one.
    """
    def findDuplicate(self, nums: List[int]) -> int:
        a = nums

        def sol1(a):
            v = {}
            n = len(a)
            for i in range(n):
                if a[i] in v: return a[i]
                v[a[i]] = 1
            return -1

        def sol2(a):
            a.sort()
            n = len(a)
            last = a[0]
            for i in range(1, n):
                if a[i] == last: return last
                last = a[i]
            return -1

        def sol3(a):
            def get_jmin(a, i):
                j, jmin = i+1, i
                while j < n:
                    if a[j] < a[jmin]: jmin = j
                    j += 1
                return jmin

            # selection sort O(n2) worst case
            i, n, last = 0, len(a), None
            while i < n:
                jmin = get_jmin(a, i)
                if jmin!=i and a[jmin] == a[i]: return a[i]
                if last is not None and last == a[jmin]: return last
                last = a[jmin]
                a[jmin] = a[i]
                i += 1
            return -1

        def sol4(a):
            """considered the indirect indexing scheme nums[nums[i]] using the value
               as the index and if we ever hit the same bucket again, we found a
               duplicate -- but some events obstructed the realization of this solution,
               so ended up quickly coding the above"""
            pass
        return sol2(a)





