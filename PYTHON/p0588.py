class FileSystem:
    ''' Design an in-memory file system to simulate the following functions:
        ls: Given a path in string format. If it is a file path, return a list
        that only contains this file's name. If it is a directory path, return
        the list of file and directory names in this directory. Your output
        (file and directory names together) should in lexicographic order.

        mkdir: Given a directory path that does not exist, you should make a
        new directory according to the path. If the middle directories in the
        path don't exist either, you should create them as well. This function
        has void return type.

        addContentToFile: Given a file path and file content in string format.
        If the file doesn't exist, you need to create that file containing given
        content. If the file already exists, you need to append given content
        to original content. This function has void return type.

        readContentFromFile: Given a file path, return its content in string format.
    '''
    def __init__(self):
        import os
        self.dirs = {}
        self.files = {}
        self.contents = {}
        self.dirs["/"], self.contents["/"] =  True, []
        return


    def update(self, what, stuff):
        if what not in self.contents:
            self.contents[what] = []
        if stuff not in self.contents[what]:
            self.contents[what].append(stuff)
        return


    def ls(self, path: str) -> List[str]:
        if path in self.dirs:
            return sorted(self.contents[path])
        if path in self.files:
            return [os.path.basename(path)]


    def fsdump(self):
        print('-'*32)
        print('files', self.files)
        print('dirs', self.dirs)
        print('contents', self.contents)
        return


    def mkdir(self, path: str) -> None:
        if path not in self.dirs and path not in self.files:
            parent = ""
            for cdir in path.split("/"):
                cpath = "/".join([parent, cdir]) if parent != "/" else "/" + cdir
                if cpath not in self.dirs:
                    self.dirs[cpath] = True
                    self.contents[cpath] = []
                self.update(parent,cdir)
                self.update(cdir, [])
                parent = cpath
                #self.fsdump()
        return


    def addContentToFile(self, filePath: str, content: str) -> None:
        parent = os.path.dirname(filePath)
        stem = os.path.basename(filePath)
        self.mkdir(parent)
        self.update(parent, stem)
        if filePath not in self.files: self.files[filePath] = True
        self.update(filePath, content)
        #self.fsdump()
        return


    def readContentFromFile(self, filePath: str) -> str:
        if filePath in self.contents:
            return "".join(self.contents[filePath])
        return ""


# Your FileSystem object will be instantiated and called as such:
# obj = FileSystem()
# param_1 = obj.ls(path)
# obj.mkdir(path)
# obj.addContentToFile(filePath,content)
# param_4 = obj.readContentFromFile(filePath)
