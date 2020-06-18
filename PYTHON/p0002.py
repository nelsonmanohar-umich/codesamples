# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, x):
#         self.val = x
#         self.next = None

class Solution:
    ''' You are given two non-empty linked lists representing two non-negative integers.
        The digits are stored in reverse order and each of their nodes contain a single
        digit. Add the two numbers and return it as a linked list.

        You may assume the two numbers do not contain any leading zero, except the number
        0 itself.
    '''
    def addTwoNumbers(self, l1: ListNode, l2: ListNode) -> ListNode:

        res_head = None

        p, q = l1, l2

        carry = 0
        prev = None
        while p or q:
            val = ((p.val if p else 0) + (q.val if q else 0) + carry)
            adder = val % 10
            carry = val //10
            # print( adder, carry)
            new_val = ListNode(adder)
            if res_head is None:
                res_head = new_val
            if prev is not None:
                prev.next = new_val
            prev = new_val
            p, q = p.next if p else None, q.next if q else None

        if carry:
            new_val = ListNode(carry)
            prev.next = new_val

        return res_head


