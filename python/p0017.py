class Solution:
    ''' Given a string containing digits from 2-9 inclusive,
        return all possible letter combinations that the number
        could represent.

        A mapping of digit to letters (just like on the telephone
        buttons) is given below. Note that 1 does not map to any letters.
    '''
    def letterCombinations(self, digits: str) -> List[str]:

        digits = [d for d in digits if d in "23456789"]
        n = len(digits)
        if n == 0:
            return []

        mappings = {'2': "abc",
                    '3': "def",
                    '4': "ghi",
                    '5': "jkl",
                    '6': "mno",
                    '7': "pqrs",
                    '8': "tuv",
                    '9': "wxyz",
                    '0': "",
                    '1': ""}

        def dfs(nums, path="", paths={}, visited={}):
            key = (path, len(nums))
            if key in visited:
                return
            visited[key] = True

            if not nums:
                paths[path] = True
                return

            next_digit = nums[0]
            for letter in mappings.get(next_digit, ""):
                if letter:
                    dfs(nums[1:], path+letter, paths, visited)

            return

        paths = {}
        visited = {}
        dfs(digits, "", paths, visited)
        return list(paths.keys())
