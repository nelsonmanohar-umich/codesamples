class Solution:
    ''' The string "PAYPALISHIRING" is written in a zigzag
        pattern on a given number of rows like this:
            P   A   H   N
            A P L S I I G
            Y   I   R
        And then read line by line: "PAHNAPLSIIGYIR"

        Write the code that will take a string and make
        this conversion given a number of rows:
    '''
    def convert(self, s: str, numRows: int) -> str:
        n = len(s)
        nr = numRows
        if nr == 0:
            return ""
        if nr == 1:
            return s

        nc = (n-1)*n//(nr+nr-2) + 1
        mat = [["" for j in range(nc)] for i in range(nr)]

        i, j = (0, 0)
        ix = 0
        while ix < n:
            for i in range(0,nr):
                if ix < n:
                    mat[i][j] = s[ix]
                else:
                    break
                ix += 1
            i, j = nr-1, j

            if ix >= n:
                break

            jj = 0
            for jj in range(1,nr-1):
                if ix < n:
                    mat[i-jj][j+jj] = s[ix]
                else:
                    break
                ix += 1

            i, j = 0, j+jj+1

        ss = ""
        for row in mat:
            ss += "".join(x for x in row if x)
        return ss




