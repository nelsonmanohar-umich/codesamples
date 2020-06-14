class Solution:
    ''' There are two sorted arrays nums1 and nums2 of size m and n respectively.
        Find the median of the two sorted arrays. The overall run time complexity
        should be O(log (m+n)).

        You may assume nums1 and nums2 cannot be both empty.
    '''
    def findMedianSortedArrays(self, nums1: List[int], nums2: List[int]) -> float:

        n1 = len(nums1)
        n2 = len(nums2)
        if n1 == 0 and n2 == 0:
            return 0
        elif n2 == 0:
            med = (n1//2, n1//2) if n1 % 2 else (n1//2-1, n1//2)
            median = (nums1[med[0]] + nums1[med[1]])/2.
            return median
        elif n1 == 0:
            med = (n2//2, n2//2) if n2 % 2 else (n2//2-1, n2//2)
            median = (nums2[med[0]] + nums2[med[1]])/2.
            return median

        m = n1 + n2
        med = (m//2, m//2) if m % 2 else (m//2-1, m//2)
        #print(med, m, n1, n2)

        res = []
        i, j = 0, 0
        for pos in range(m):
            v1 = nums1[i] if i < n1 else None
            v2 = nums2[j] if j < n2 else None
            if v1 is not None and v2 is not None:
                if v1 < v2:
                    i += 1
                    res.append(v1)
                else:
                    j += 1
                    res.append(v2)
            elif v1 is not None:
                res.append(v1)
                i += 1
            elif v2 is not None:
                res.append(v2)
                j += 1

            if pos > med[1]:
                break

        median = (res[med[0]] + res[med[1]])/2.

        return median

