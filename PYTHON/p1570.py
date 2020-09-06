class SparseVector:
   '''Given two sparse vectors, compute their dot product.  Implement class SparseVector:
      SparseVector(nums) Initializes the object with the vector nums dotProduct(vec)
      Compute the dot product between the instance of SparseVector and vec

      A sparse vector is a vector that has mostly zero values, you should store the sparse
      vector efficiently and compute the dot product between two SparseVector.

      Follow up: What if only one of the vectors is sparse?''
    '''
    def __init__(self, nums: List[int]):
        self.vals = {}
        self.n = len(nums)
        for i,num in enumerate(nums):
            if num:
                self.vals[i] = num
        self.ixs = set(list(self.vals.keys()))
        return



    # Return the dotProduct of two sparse vectors
    def dotProduct(self, vec: 'SparseVector') -> int:
        i2 = vec.ixs
        i1 = self.ixs
        intersection = i1 & i2
        res = 0
        for ix in intersection:
            res += vec.vals[ix] * self.vals[ix]
        return res


# Your SparseVector object will be instantiated and called as such:
# v1 = SparseVector(nums1)
# v2 = SparseVector(nums2)
# ans = v1.dotProduct(v2)
