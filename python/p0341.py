# """
# This is the interface that allows for creating nested lists.
# You should not implement it, or speculate about its implementation
# """
#class NestedInteger:
#    def isInteger(self) -> bool:
#        """
#        @return True if this NestedInteger holds a single integer, rather than a nested list.
#        """
#
#    def getInteger(self) -> int:
#        """
#        @return the single integer that this NestedInteger holds, if it holds a single integer
#        Return None if this NestedInteger holds a nested list
#        """
#
#    def getList(self) -> [NestedInteger]:
#        """
#        @return the nested list that this NestedInteger holds, if it holds a nested list
#        Return None if this NestedInteger holds a single integer
#        """

class NestedIterator:
    '''
    Given a nested list of integers, implement an iterator to flatten it.
    Each element is either an integer, or a list -- whose elements may also be integers or other lists.
    Example 1:
    Input: [[1,1],2,[1,1]]
    Output: [1,1,2,1,1]
    Explanation: By calling next repeatedly until hasNext returns false,
                 the order of elements returned by next should be: [1,1,2,1,1].

    '''
    def __init__(self, nestedList: [NestedInteger]):
        self.items, self.n, self.i = [], 0, 0
        for item in nestedList:
            #print('-'*32)
            #print("TOP", item)
            subseq = self.handle_item(item, [])
            for subitem in subseq:
                self.items.append(subitem)
        return


    def handle_item(self, item, ret=[]):
        if not item.isInteger():
            for subitem in item.getList():
                self.handle_item(subitem, ret)
        else:
            ret.append(item.getInteger())
            self.n += 1
        return ret


    def next(self) -> int:
        item = self.items[self.i]
        #print('returning', item, self.i, self.n)
        self.i += 1
        return item



    def hasNext(self) -> bool:
        return self.i < self.n



# Your NestedIterator object will be instantiated and called as such:
# i, v = NestedIterator(nestedList), []
# while i.hasNext(): v.append(i.next())
