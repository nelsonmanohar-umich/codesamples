class Solution:
    # https://leetcode.com/problems/spiral-matrix-ii/
    def generateMatrix(self, n: int) -> List[List[int]]:
        left, right, top, bottom = 0, n-1, 0, n-1

        mat = [[0 for j in range(n)] for i in range(n)]
        cnt = 1
        limit = n*n

        def state():
            for row in mat:
                print (row)
            print(top, bottom, left, right)
            print(cnt, limit)
            print('-' * 32)

        while cnt <= limit:
            if 0 <= top <= n:
                for j in range(left, right+1):
                    mat[top][j] = cnt
                    cnt += 1
                top += 1

            # state()
            if cnt > limit: break
            if 0 <= right < n:
                for i in range(top, bottom+1):
                    mat[i][right] = cnt
                    cnt += 1
                right -= 1

            # state()
            if cnt > limit: break
            if 0 <= bottom <= n:
                for j in range(left, right+1)[::-1]:
                    mat[bottom][j] = cnt
                    cnt += 1
                bottom -= 1

            # state()
            if cnt > limit: break
            if 0 <= left <= n:
                for i in range(top, bottom+1)[::-1]:
                    mat[i][left] = cnt
                    cnt += 1
                left += 1
            # state()

        return mat

