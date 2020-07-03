class TimeMap:
    '''
    Create a timebased key-value store class TimeMap, that supports two operations.
    1. set(string key, string value, int timestamp)
       Stores the key and value, along with the given timestamp.
    2. get(string key, int timestamp)
       Returns a value such that set(key, value, timestamp_prev) was called previously, with timestamp_prev <= timestamp.
       If there are multiple such values, it returns the one with the largest timestamp_prev.
       If there are no values, it returns the empty string ("").
    Example 1:
       Input: inputs = ["TimeMap","set","get","get","set","get","get"], inputs = [[],["foo","bar",1],["foo",1],["foo",3],["foo","bar2",4],["foo",4],["foo",5]]
       Output: [null,null,"bar","bar",null,"bar2","bar2"]
    '''
    def __init__(self):
        """
        Initialize your data structure here.
        """
        from bisect import bisect_right
        self.cache = {}
        return


    def set(self, key: str, value: str, timestamp: int) -> None:
        # asssumes time is monotonically increasing in the stream presented by set()
        if key not in self.cache: self.cache[key] = {'t': [], 'v': []}
        self.cache[key]['t'].append(timestamp)
        self.cache[key]['v'].append(value)
        return


    def get(self, key: str, timestamp: int) -> str:
        if key in self.cache:
            values = self.cache[key]['t']
            ix = bisect_right(values, timestamp)
            if 0 <= ix-1 < len(values):
                return self.cache[key]['v'][ix-1]
            else:
                return ""
        else:
            return ""



# Your TimeMap object will be instantiated and called as such:
# obj = TimeMap()
# obj.set(key,value,timestamp)
# param_2 = obj.get(key,timestamp)
