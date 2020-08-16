class Solution:
    '''
    Given a list of directory info including directory path, and all
    the files with contents in this directory, you need to find out
    all the groups of duplicate files in the file system in terms of
    their paths.

    A group of duplicate files consists of at least two files that
    have exactly the same content.

    A single directory info string in the input list has the following
    format:
          "root/d1/d2/.../dm f1.txt(f1_content)
                             f2.txt(f2_content) ... fn.txt(fn_content)"

    It means there are n files (f1.txt, f2.txt ... fn.txt with content
    f1_content, f2_content ... fn_content, respectively) in directory
    root/d1/d2/.../dm. Note that n >= 1 and m >= 0. If m = 0, it means
    the directory is just the root directory.

    The output is a list of group of duplicate file paths. For each group,
    it contains all the file paths of the files that have the same content.
    A file path is a string that has the following format:
        "directory_path/file_name.txt"
    '''
    def findDuplicate(self, paths: List[str]) -> List[List[str]]:
        import os.path
        import re

        n = len(paths)
        if n == 0: return []

        p = re.compile("(.*\..*)\((.*)\)")

        contents = {}
        for dir_entry in paths:
            items = dir_entry.split(" ")
            path, file_entries = items[0], items[1:]
            for file_entry in file_entries:
                matches = p.findall(file_entry)
                if matches:
                    file, content = matches[0]
                    if content not in contents: contents[content] = []
                    data = "%s/%s" % (path, file)
                    contents[content].append(data)

        res = []
        for content in contents:
            if len(contents[content]) > 1:
                res.append(contents[content])

        return res



