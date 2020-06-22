# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, val=0, next=None):
#         self.val = val
#         self.next = next
class Solution:
    '''
    Given a non-negative integer represented as non-empty a singly linked list of digits,
    plus one to the integer.

    You may assume the integer do not contain any leading zero, except the number 0 itself.
    The digits are stored such that the most significant digit is at the head of the list.
    Example :
       Input: [1,2,3]
       Output: [1,2,4]

    '''
    def plusOne(self, head: ListNode) -> ListNode:
        p = head
        n = 0
        def visit(p):
            if p is None:
                return 1
            val = p.val + visit(p.next)
            d, c = val % 10, val // 10
            p.val = d
            return c

        c = visit(head)
        if c == 1:
            p = ListNode(val=1, next=head)
            head = p

        # p = head
        # digits = []
        # while p is not None:
            # digits.append(p.val)
            # p = p.next

        return head

