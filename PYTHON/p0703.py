class KthLargest:
    '''
    Design a class to find the kth largest element in a stream. Note that
    it is the kth largest element in the sorted order, not the kth distinct element.
    Your KthLargest class will have a constructor which accepts an integer
    k and an integer array nums, which contains initial elements from the
    stream. For each call to the method KthLargest.add, return the element
    representing the kth largest element in the stream.

    Example:
    int k = 3;
    int[] arr = [4,5,8,2];
    KthLargest kthLargest = new KthLargest(3, arr);
    kthLargest.add(3);   // returns 4
    kthLargest.add(5);   // returns 5
    kthLargest.add(10);  // returns 5
    kthLargest.add(9);   // returns 8
    kthLargest.add(4);   // returns 8
    Note:
    You may assume that nums' length ≥ k-1 and k ≥ 1.
    '''

    def __init__(self, k: int, nums: List[int]):
        self.items = sorted(nums)
        self.k = k
        return

    def add(self, val: int) -> int:
        if not self.items:
            self.items.append(val)
            return val

        n = len(self.items)
        lo, hi = 0, n-1
        while lo <= hi:
            mi = lo + (hi-lo)//2
            if self.items[mi] == val:
                break
            elif val > self.items[mi]:
                lo, hi = mi+1, hi
            else: # val < self.items[mi]:
                lo, hi = lo, mi-1

        if val <= self.items[mi]:
            self.items.insert(mi, val)
        else:
            self.items.insert(mi+1, val)

        return self.items[-self.k]


# Your KthLargest object will be instantiated and called as such:
# obj = KthLargest(k, nums)
# param_1 = obj.add(val)
