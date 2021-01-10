class Solution:
    '''
    Given an encoded string, return its decoded string.
    The encoding rule is: k[encoded_string], where the encoded_string
    inside the square brackets is being repeated exactly k times.
    Note that k is guaranteed to be a positive integer.
    You may assume that the input string is always valid;
    No extra white spaces, square brackets are well-formed, etc.
    Furthermore, you may assume that the original data does not
    contain any digits and that digits are only for those repeat
    numbers, k. For example, there won't be input like 3a or 2[4].

    this implementation ran against some network delay and got very imprecise numbers.
    it will definitevely be slower than the stack (because of the regular expression),
    but it is easier to review.
    '''
    def decodeString(self, s: str) -> str:
        p = re.compile("[\d]{1,3}\[\w*\]")
        g = re.compile("([\d]{1,3})\[(\w*)\]")

        def expand_group(group):
            repeat, what = g.findall(group)[0]
            return what * int(repeat)

        def replace_group(s, group):
            ix = s.find(group)
            out = expand_group(group)
            m = len(group)
            return s[0:ix] + out + s[ix+m:]

        def expand_string(s):
            groups = p.findall(s)
            while groups:
                group = groups.pop()
                s = replace_group(s, group)
            return s

        while "[" in s or "]" in s:
            s = expand_string(s)

        return s


