class MyHashMap:
    '''
    Design a HashMap without using any built-in hash table libraries.
    To be specific, your design should include these functions:
      put(key, value) : Insert a (key, value) pair into the HashMap. If
                        the value already exists in the HashMap, update the value.
      get(key): Returns the value to which the specified key is mapped,
                        or -1 if this map contains no mapping for the key.
      remove(key) : Remove the mapping for the value key if this map contains
                        the mapping for the key.

    Example:
    MyHashMap hashMap = new MyHashMap();
    hashMap.put(1, 1);
    hashMap.put(2, 2);
    hashMap.get(1);            // returns 1
    hashMap.get(3);            // returns -1 (not found)
    hashMap.put(2, 1);          // update the existing value
    hashMap.get(2);            // returns 1
    hashMap.remove(2);          // remove the mapping for 2
    hashMap.get(2);            // returns -1 (not found)

    Note:
    All keys and values will be in the range of [0, 1000000].
    The number of operations will be in the range of [1, 10000].
    Please do not use the built-in HashMap library.
    '''

    def __init__(self):
        """
        Initialize your data structure here.
        """
        self.size = 100
        self.mappings = [None]*self.size
        return


    def put(self, key: int, value: int) -> None:
        """
        value will always be non-negative.
        """
        mkey = key % self.size
        bucket = self.mappings[mkey]
        if bucket is None:
            self.mappings[mkey] = []
            self.mappings[mkey].append([key, value])
            return
        else:
            for i, (rkey, rval) in enumerate(self.mappings[mkey]):
                if rkey == key:
                    self.mappings[mkey][i] = [key, value]
                    return
            self.mappings[mkey].append([key, value])
        return



    def get(self, key: int) -> int:
        """
        Returns the value to which the specified key is mapped, or -1 if this map contains no mapping for the key
        """
        mkey = key % self.size
        mvals = self.mappings[mkey]
        if mvals is None:
            return -1
        for rkey, mval in mvals:
            if rkey == key:
                return mval
        return -1


    def remove(self, key: int) -> None:
        """
        Removes the mapping of the specified value key if this map contains a mapping for the key
        """
        mkey = key % self.size
        mvals = self.mappings[mkey]
        if mvals is None: return
        for i, (rkey, mval) in enumerate(mvals):
            if rkey == key:
                self.mappings[mkey].pop(i)
                return
        return



# Your MyHashMap object will be instantiated and called as such:
# obj = MyHashMap()
# obj.put(key,value)
# param_2 = obj.get(key)
# obj.remove(key)
