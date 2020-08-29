class FileSystem:
    '''
    You are asked to design a file system which provides two functions:
        createPath(path, value):
        Creates a new path and associates a value to it if possible and returns True.
        Returns False if the path already exists or its parent path doesn't exist.

        get(path):
        Returns the value associated with a path or returns -1 if the path doesn't exist.

        The format of a path is one or more concatenated strings of the form: / followed
        by one or more lowercase English letters. For example, /leetcode and /leetcode/problems
        are valid paths while an empty string and / are not.  Implement the two functions.
    '''
    def __init__(self):
        self.contents = {}
        return


    def is_valid(self, path):
        items = [x for x in path.split("/")]
        start, middle, stem = items[0], items[1:-1], items[-1]
        if start != "": return False, None
        if middle:
            path = start
            for item in middle:
                path += "/%s" % item
                if path not in self.contents:
                    return False, None
        return True, stem


    def createPath(self, path: str, value: int) -> bool:
        valid, stem = self.is_valid(path)
        if not valid: return False
        if path in self.contents and self.contents[path] != value: return False
        self.contents[path] = value
        return True


    def get(self, path: str) -> int:
        valid, stem = self.is_valid(path)
        return -1 if (not valid or path not in self.contents) else self.contents[path]



# Your FileSystem object will be instantiated and called as such:
# obj = FileSystem()
# param_1 = obj.createPath(path,value)
# param_2 = obj.get(path)
