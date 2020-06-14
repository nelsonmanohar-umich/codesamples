class Solution:
    '''
    Given two non-negative integers num1 and num2 represented as
    string, return the sum of num1 and num2.

    Note:
    The length of both num1 and num2 is < 5100.
    Both num1 and num2 contains only digits 0-9.
    Both num1 and num2 does not contain any leading zero.
    You must not use any built-in BigInteger library or convert the inputs to integer directly.
    '''
    def addStrings(self, num1: str, num2: str) -> str:


        def addstr(s1, s2):
            total = ""
            carry = 0
            pairings = list(zip(range(len(s1)), s1, s2))
            for i, x1, x2 in pairings[::-1]:
                x1, x2 = int(x1), int(x2)
                xsum = (x1 + x2 + carry) % 10
                carry = 1 if (x1+x2+carry) >= 10 else 0
                total += str(xsum)
            total = total[::-1]
            if carry:
                total = "1" + total
            return total


        n1, n2 = len(num1), len(num2)
        num1, num2 = (num1, num2) if n1 >= n2 else (num2, num1)
        n1, n2 = len(num1), len(num2)

        num2 = "0"*(n1-n2)+num2
        #print(num1, num2)
        assert len(num1) == len(num2)

        return  addstr(num1, num2)

