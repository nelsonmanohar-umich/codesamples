class MyCircularQueue:
    # https://leetcode.com/problems/design-circular-queue/
    def __init__(self, k: int):
        """
        Initialize your data structure here. Set the size of the queue to be k.
        """
        self.k = k
        self.tail = 0
        self.head = 0
        self.size = 0
        self.data = [None] * k
        return


    def where(self, ix):
        return ix % self.k


    def enQueue(self, value: int) -> bool:
        """
        Insert an element into the circular queue. Return true if the operation is successful.
        """
        if self.size >= self.k:
            return False
        self.tail = self.where(self.tail)
        self.data[self.tail] = value
        self.size += 1
        self.tail += 1
        self.tail = self.where(self.tail)
        return True


    def deQueue(self) -> bool:
        """
        Delete an element from the circular queue. Return true if the operation is successful.
        """
        if self.size == 0: return False
        #value = self.data[self.head]
        self.size -= 1
        self.data[self.head] = None
        self.head = self.where(self.head+1)
        return True


    def Front(self) -> int:
        """
        Get the front item from the queue.
        """
        if self.size == 0: return -1
        value = self.data[self.head]
        return value


    def Rear(self) -> int:
        """
        Get the last item from the queue.
        """
        if self.size == 0: return -1
        return self.data[self.where(self.tail-1)]


    def isEmpty(self) -> bool:
        """
        Checks whether the circular queue is empty or not.
        """
        if self.size == 0: return True
        return False


    def isFull(self) -> bool:
        """
        Checks whether the circular queue is full or not.
        """
        if self.size == self.k: return True
        return False


# Your MyCircularQueue object will be instantiated and called as such:
# obj = MyCircularQueue(k)
# param_1 = obj.enQueue(value)
# param_2 = obj.deQueue()
# param_3 = obj.Front()
# param_4 = obj.Rear()
# param_5 = obj.isEmpty()
# param_6 = obj.isFull()
