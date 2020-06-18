class LRUCache:
    '''
    Design and implement a data structure for Least
    Recently Used (LRU) cache. It should support the
    following operations: get and put.

    get(key) - Get the value (will always be positive) of the
               key if the key exists in the cache, otherwise
               return -1.
    put(key, value) - Set or insert the value if the key is
               not already present. When the cache reached its
               capacity, it should invalidate the least recently
               used item before inserting a new item.

    The cache is initialized with a positive capacity.

    Follow up:
    Could you do both operations in O(1) time complexity?

    Example:
    LRUCache cache = new LRUCache( 2 /* capacity */ );

    cache.put(1, 1);
    cache.put(2, 2);
    cache.get(1);       // returns 1
    cache.put(3, 3);    // evicts key 2
    cache.get(2);       // returns -1 (not found)
    cache.put(4, 4);    // evicts key 1
    cache.get(1);       // returns -1 (not found)
    cache.get(3);       // returns 3
    cache.get(4);       // returns 4
    '''

    def __init__(self, capacity: int):
        from collections import deque
        self.n = capacity
        self.q = deque()
        self.cnt = 0
        self.cache = {}
        return


    def get(self, key: int) -> int:
        if key in self.cache:
            self.q.remove(key)
            self.q.append(key)
            return self.cache[key]
        return -1


    def put(self, key: int, value: int) -> None:
        if self.cnt < self.n and key not in self.cache:
            self.cnt += 1
            self.cache[key] = value
            self.q.append(key)
            return

        if key in self.cache:
            self.get(key)
            self.cache[key] = value
            return

        old_key = self.q.popleft()
        del self.cache[old_key]

        self.q.append(key)
        self.cache[key] = value

        return


# Your LRUCache object will be instantiated and called as such:
# obj = LRUCache(capacity)
# param_1 = obj.get(key)
# obj.put(key,value)
