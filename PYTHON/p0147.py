# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, val=0, next=None):
#         self.val = val
#         self.next = next
class Solution:
    '''
    Algorithm of Insertion Sort: -  Sort a linked list using insertion sort.
    Insertion sort iterates, consuming one input element each repetition, and growing a sorted
    output list.  At each iteration, insertion sort removes one element from the input data,
    finds the location it belongs within the sorted list, and inserts it there.  It repeats
    until no input elements remain.
    '''
    def insertionSortList(self, head: ListNode) -> ListNode:
        # lazy scheme given requirements did not specify auxiliary storage limitations

        # do a linear scane to memoize the nodes
        p = head
        if not head: return head

        nodes = []
        while p:
            nodes.append(p)
            p = p.next

        if len(nodes) == 1:
            return head

        # do insertion sort on the memoized nodes
        already_sorted = [nodes[0]]
        n = len(nodes)
        for i in range(1,n):
            ix = None
            for j in range(len(already_sorted)):
                if already_sorted[j].val >= nodes[i].val:
                    ix = j
                    break
            if ix is not None:
                already_sorted.insert(j, nodes[i])
            else:
                already_sorted.append(nodes[i])

        # replace the values of the linked list
        already_sorted.reverse()
        head = already_sorted.pop()
        p = head
        while already_sorted:
            pp = already_sorted.pop()
            p.next = pp
            pp.next = None
            p = pp

        return head





