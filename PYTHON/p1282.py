class Solution:
    '''
    There are n people whose IDs go from 0 to n - 1 and each person belongs
    exactly to one group. Given the array groupSizes of length n telling
    the group size each person belongs to, return the groups there are and
    the people's IDs each group includes.

    You can return any solution in any order and the same applies for IDs.
    Also, it is guaranteed that there exists at least one solution.

    Example 1:
    Input: groupSizes = [3,3,3,3,3,1,3]
    Output: [[5],[0,1,2],[3,4,6]]
    Explanation:
    Other possible solutions are [[2,1,6],[5],[0,4,3]] and [[5],[0,6,2],[4,3,1]].
    '''
    def groupThePeople(self, groupSizes: List[int]) -> List[List[int]]:
        gs = groupSizes
        groups = {}
        for i, size in enumerate(gs):
            if size not in groups: groups[size] = []
            groups[size].append(i)

        res = []
        keys = sorted(groups.keys())
        for size in keys:
            m = len(groups[size])
            for i in range(0, m//size):
                lo, hi = i*size, (i+1)*size
                members = groups[size][lo:hi]
                res.append(members)
        return res


