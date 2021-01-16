class Solution:
    '''
    https://leetcode.com/problems/minimum-remove-to-make-valid-parentheses/
    Given a string s of '(' , ')' and lowercase English characters.

    Your task is to remove the minimum number of parentheses ( '(' or ')',
    in any positions ) so that the resulting parentheses string is valid
    and return any valid string.

    Formally, a parentheses string is valid if and only if:

    It is the empty string, contains only lowercase characters, or
    It can be written as AB (A concatenated with B), where A and B are
    valid strings, or It can be written as (A), where A is a valid string.
    '''
    def minRemoveToMakeValid(self, s: str) -> str:
        stack = []
        opens, closes = 0, 0
        remove = {}
        for i, c in enumerate(s):
            if c == "(":
                stack.append([c, i])
                opens += 1
            elif c == ")":
                if opens == 0:
                    remove[i] = c
                    continue
                else:
                    closes += 1
                    if stack:
                        closes += 1
                        stack.pop()
                        continue
                    else:
                        remove[i] = c

        for c, i in stack:
            remove[i] = c

        out = ""
        for i, c in enumerate(s):
            if i not in remove:
                out += c
        return out
