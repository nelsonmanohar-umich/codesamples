class Solution:
    '''
    Compare two version numbers version1 and version2.
       If version1 > version2 return 1;
       if version1 < version2 return -1;
       otherwise return 0.

    You may assume that the version strings are non-empty
    and contain only digits and the . character.

    The . character does not represent a decimal point and
    is used to separate number sequences.

    For instance, 2.5 is not "two and a half" or "half way
    to version three", it is the fifth second-level revision
    of the second first-level revision.

    You may assume the default revision number for each level
    of a version number to be 0. For example, version number
    3.4 has a revision number of 3 and 4 for its first and
    second level revision number. Its third and fourth level
    revision number are both 0.
    '''
    def compareVersion(self, version1: str, version2: str) -> int:
        from itertools import zip_longest

        def get_elems(v):
            vals = [x.lstrip("0") if x.lstrip("0") else 0 for x in v.split(".")]
            vals = [int(x) for x in vals]
            return vals

        g1 = get_elems(version1)
        g2 = get_elems(version2)

        pairings = zip_longest(g1, g2, fillvalue=0)
        for (x, y) in pairings:
            print(x, y)
            if x is not None and y is not None and x > y: return 1
            if x is not None and y is not None and x < y: return -1
            if x is None: return -1
            if y is None: return 1
        return 0



