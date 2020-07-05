class Solution:
    '''
    Given an array of words and a width maxWidth, format the text such that each
    line has exactly maxWidth characters and is fully (left and right) justified.
    You should pack your words in a greedy approach; that is, pack as many words
    as you can in each line. Pad extra spaces ' ' when necessary so that each line
    has exactly maxWidth characters.
    Extra spaces between words should be distributed as evenly as possible. If the
    number of spaces on a line do not divide evenly between words, the empty slots
    on the left will be assigned more spaces than the slots on the right.
    For the last line of text, it should be left justified and no extra space is
    inserted between words.
    Note:
    A word is defined as a character sequence consisting of non-space characters only.
    Each word's length is guaranteed to be greater than 0 and not exceed maxWidth.
    The input array words contains at least one word.
    '''
    def fullJustify(self, words: List[str], maxWidth: int) -> List[str]:
        N = maxWidth
        lines, line = [], ""
        for word in words:
            proto = line + " " + word if line else word
            if len(proto) <= N:
                line = proto
            else:
                nchars = len(line)
                num_spaces = len(line.split()) - 1
                if num_spaces != 0:
                    common_padding = 1 + (N-nchars) // (num_spaces)
                    remain_padding = (N-nchars) % (num_spaces)
                else:
                    common_padding = 0
                    remain_padding = N - len(line.strip())

                nline = ""
                lwords = line.split(" ")
                for i, w in enumerate(lwords[:-1]):
                    nline += w + " " *(common_padding) + (" " if i < remain_padding else "")
                nline += lwords[-1] + ("" if num_spaces != 0 else " " * remain_padding)
                lines.append(nline)
                line = word

        if line:
            final_padding = " " * (N - len(line))
            lines.append(line + final_padding)

        return lines


