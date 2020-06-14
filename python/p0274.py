class Solution:
    '''
    Given an array of citations (each citation is a non-negative integer)
    of a researcher, write a function to compute the researcher's h-index.

    According to the definition of h-index on Wikipedia: "A scientist has
    index h if h of his/her N papers have at least h citations each, and
    the other N − h papers have no more than h citations each."

    Example:
    Input: citations = [3,0,6,1,5]
    Output: 3
    Explanation: [3,0,6,1,5] means the researcher has 5 papers in total and each of them had
                 received 3, 0, 6, 1, 5 citations respectively.
                 Since the researcher has 3 papers with at least 3 citations each and the remaining
                 two with no more than 3 citations each, her h-index is 3.
    '''
    def hIndex(self, citations: List[int]) -> int:

        n = len(citations)
        if n == 1:
            return min(citations[0],1)

        for i in range(n+1)[::-1]:
            h = len([citations[k] for k in range(n) if citations[k] >= i])
            if h >= i:
                return i
        return 0
