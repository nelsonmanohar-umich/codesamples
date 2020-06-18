# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, x):
#         self.val = x
#         self.next = None

class Solution:
    '''
    Merge k sorted linked lists and return it as one sorted
    list. Analyze and describe its complexity.

    Example:
        Input:
        [
          1->4->5,
          1->3->4,
          2->6
            ]
        Output: 1->1->2->3->4->4->5->6
    '''
    def mergeKLists(self, lists: List[ListNode]) -> ListNode:
        # curr = [0 for x in range(k)]

        items = []
        for l in lists:
            if l:
                v = l.val
                items.append((v, l))
                l = l.next

        p = ListNode()
        head = p
        while items:
            #print('-'*32)
            #print([(v, l.val) for v, l in items])
            ix = items.index(min(items, key=lambda x: x[0]))
            (minval, l) = items[ix]
            #print(ix, minval, l.val)
            items.pop(ix)
            p.next = ListNode(minval)
            p = p.next
            if l:
                l = l.next
                if l:
                    v = l.val
                    items.append((v, l))

        return head.next
