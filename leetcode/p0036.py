class Solution:
    '''
    Determine if a 9x9 Sudoku board is valid. Only the filled cells
    need to be validated according to the following rules:

    Each row must contain the digits 1-9 without repetition.
    Each column must contain the digits 1-9 without repetition.
    Each of the 9 3x3 sub-boxes of the grid must contain the digits
    1-9 without repetition.
    '''
    def isValidSudoku(self, board: List[List[str]]) -> bool:
        from collections import Counter

        def valid_board():
            for i in range(n):
                for j in range(n):
                    if board[i][j] != ".": return True
            return False

        def valid_row(i):
            counts = Counter(board[i])
            for key in counts:
                if key == ".": continue
                if counts[key] > 1: return False
            return True

        def valid_col(j):
            counts = Counter([board[i][j] for i in range(n)])
            for key in counts:
                if key == ".": continue
                if counts[key] > 1: return False
            return True

        def valid_square(i, j):
            counts = {}
            for ii in range(0,3):
                for jj in range(0,3):
                    val = board[i+ii][j+jj]
                    if val == ".": continue
                    if val not in counts: counts[val] = 0
                    counts[val] += 1
                    if counts[val] > 1:
                        return False
            return True


        n = len(board)
        #for i in range(n): print (board[i])

        for i in range(n):
            if not valid_row(i):
                return False
        #print("Valid Rows")

        for j in range(n):
            if not valid_col(j):
                return False
        #print("Valid Cols")

        for i in range(0,n,3):
            for j in range(0,n,3):
                if not valid_square(i,j):
                    return False
        #print ("Valid Squares")
        return True
