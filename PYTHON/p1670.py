class FrontMiddleBackQueue:

    # https://leetcode.com/problems/design-front-middle-back-queue/
    def __init__(self):
        self.data = deque()
        return


    def pushFront(self, val: int) -> None:
        self.data.appendleft(val)
        return


    def which(self):
        m = len(self.data)
        return m//2


    def pushMiddle(self, val: int) -> None:
        ix = self.which()
        self.data.insert(ix,val)


    def pushBack(self, val: int) -> None:
        self.data.append(val)


    def popFront(self) -> int:
        if not self.data: return -1
        return self.data.popleft()


    def popMiddle(self) -> int:
        if not self.data: return -1
        ix = self.which()
        if len(self.data) % 2 == 0:
            ix -= 1
        val = self.data[ix]
        del self.data[ix]
        return val


    def popBack(self) -> int:
        if not self.data: return -1
        return self.data.pop()



# Your FrontMiddleBackQueue object will be instantiated and called as such:
# obj = FrontMiddleBackQueue()
# obj.pushFront(val)
# obj.pushMiddle(val)
# obj.pushBack(val)
# param_4 = obj.popFront()
# param_5 = obj.popMiddle()
# param_6 = obj.popBack()
