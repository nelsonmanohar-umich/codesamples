# """
# This is the ImmutableListNode's API interface.
# You should not implement it, or speculate about its implementation.
# """
# class ImmutableListNode:
#     def printValue(self) -> None: # print the value of this node.
#     def getNext(self) -> 'ImmutableListNode': # return the next node.

class Solution:
    '''
    You are given an immutable linked list, print out all values of each node in reverse with the
    help of the following interface:
    ImmutableListNode: An interface of immutable linked list, you are given the head of the list.
    You need to use the following functions to access the linked list (you can't access the
    ImmutableListNode directly):
        ImmutableListNode.printValue(): Print value of the current node.
        ImmutableListNode.getNext(): Return the next node.
    The input is only given to initialize the linked list internally. You must solve this problem
    without modifying the linked list. In other words, you must operate the linked list using only
    the mentioned APIs.

    Follow up:
    Could you solve this problem in:
        Constant space complexity?
        Linear time complexity and less than linear space complexity?
    '''
class Solution:
    def printLinkedListInReverse(self, head: 'ImmutableListNode') -> None:
        def linear_space_time():
            stack = []
            p = head
            while p:
                stack.append(p)
                p = p.getNext()

            while len(stack):
                p = stack.pop()
                p.printValue()
            return

        def constact_space(goal):
            p = head
            prev = p
            while p != goal:
                prev = p
                p = p.getNext()
            prev.printValue()
            return prev

        return linear_space_time()

        p = constant_space(None)
        while p and p != head:
            p = constant_space(p)
        return
