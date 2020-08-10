class Solution:
    '''
    Given a picture consisting of black and white pixels, find the number of black lonely pixels.  The
    picture is represented by a 2D char array consisting of 'B' and 'W', which means black and white
    pixels respectively.  A black lonely pixel is character 'B' that located at a specific position
    where the same row and same column don't have any other black pixels.
    '''
    class Solution:
    def findLonelyPixel(self, picture: List[List[str]]) -> int:
        P = picture
        n, m = len(P), len(P[0]) if len(P) else 0

        A = [[0 if P[i][j] == 'W' else 1 for j in range(m)] for i in range(n)]
        Sr = [sum(row) for row in A]
        AT = zip(*A)
        Sc = [sum(row) for row in AT]
        #Sc = [sum([A[i][j] for i in range(n)]) for j in range(m)]

        cnt = 0
        for i in range(n):
            for j in range(m):
                if A[i][j] and Sr[i] == 1 and Sc[j] == 1:
                    cnt += 1
        return cnt
