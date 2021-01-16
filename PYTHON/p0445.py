# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, val=0, next=None):
#         self.val = val
#         self.next = next
class Solution:
    '''
    You are given two non-empty linked lists representing two non-negative integers.
    The most significant digit comes first and each of their nodes contain a single
    digit. Add the two numbers and return it as a linked list.

    You may assume the two numbers do not contain any leading zero, except the number 0 itself.
    Follow up:
    What if you cannot modify the input lists? In other words, reversing the lists is not allowed.
    '''
    def addTwoNumbers(self, l1: ListNode, l2: ListNode) -> ListNode:

        def scan(p):
            s, i = [], 0
            while p:
                s.append(p)
                p = p.next
                i += 1
            return s, i

        s1, n1 = scan(l1)
        s2, n2 = scan(l2)
        n = max(n1, n2)
        s1 = [None] * (n - n1) + s1
        s2 = [None] * (n - n2) + s2

        c = 0
        succ = None
        root = None
        for i in range(n)[::-1]:
            p1, p2 = s1[i], s2[i]
            v1, v2 = p1.val if p1 else 0, p2.val if p2 else 0
            digit = (v1 + v2 + c)
            p = ListNode(val = (digit % 10))
            c = 1 if (digit >= 10) else 0
            if succ:
                p.next = succ
                succ = p
            else:
                succ = p

        if c:
            root = ListNode(val=c)
            root.next = succ
        else:
            root = succ
        return root






