class Vector2D:
    '''
    Design and implement an iterator to flatten a 2d vector. It should support
    the following operations: next and hasNext.
    '''
    def __init__(self, v: List[List[int]]):
        self.lists = v[::-1]
        self.curr = []
        return


    def next(self) -> int:
        if self.hasNext():
            return self.curr.pop(0)
        raise StopIteration


    def get_next_list(self):
        while not self.curr:
            if self.lists:
                self.curr = self.lists.pop()
            else:
                break
        return self.curr


    def hasNext(self) -> bool:
        if self.curr:
            return True
        return self.get_next_list()



# Your Vector2D object will be instantiated and called as such:
# obj = Vector2D(v)
# param_1 = obj.next()
# param_2 = obj.hasNext()
