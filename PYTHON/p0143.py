# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, x):
#         self.val = x
#         self.next = None

class Solution:
    '''
    Given a singly linked list L: L0→L1→…→Ln-1→Ln,
    reorder it to: L0→Ln→L1→Ln-1→L2→Ln-2→…

    You may not modify the values in the list's nodes,
    only nodes itself may be changed.

    Example 1:
    Given 1->2->3->4, reorder it to 1->4->2->3.

    Example 2:
    Given 1->2->3->4->5, reorder it to 1->5->2->4->3.
    '''
    def reorderList(self, head: ListNode) -> None:
        """
        Do not return anything, modify head in-place instead.
        """

        p = head
        i = 1
        mapping = {}
        while p:
            mapping[i] = p
            i += 1
            p = p.next

        n = i-1
        i, j = 1, n
        new_head = None
        prev_ix = None
        while i <= n:
            if i % 2 == 1:
                j = i//2+1
                p = mapping[j]
                # print(i, j, prev_ix, p.val)
                if new_head is None:
                    new_head = p
                if prev_ix is not None:
                    mapping[prev_ix].next = p
                prev_ix = j
            if i % 2 == 0:
                j = n-i//2+1
                p = mapping[j]
                print(i, j, prev_ix, p.val)
                mapping[prev_ix].next = p
                prev_ix = j
            p.next = None
            i += 1
        self.head = new_head
        return






