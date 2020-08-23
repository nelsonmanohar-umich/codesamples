class AutocompleteSystem:
    '''
    Design a search autocomplete system for a search engine. Users may input a sentence
    (at least one word and end with a special character '#'). For each character they
    type except '#', you need to return the top 3 historical hot sentences that have
    prefix the same as the part of sentence already typed. Here are the specific rules:

    The hot degree for a sentence is defined as the number of times a user typed the
    exactly same sentence before.  The returned top 3 hot sentences should be sorted by
    hot degree (The first is the hottest one). If several sentences have the same degree
    of hot, you need to use ASCII-code order (smaller one appears first).

    If less than 3 hot sentences exist, then just return as many as you can.  When the
    input is a special character, it means the sentence ends, and in this case, you need
    to return an empty list.  Your job is to implement the following functions:

    The constructor function:
    AutocompleteSystem(String[] sentences, int[] times): This is the constructor.
    The input is historical data. Sentences is a string array consists of previously
    typed sentences. Times is the corresponding times a sentence has been typed.
    Your system should record these historical data.

    Now, the user wants to input a new sentence. The following function will provide
    the next character the user types:

    List<String> input(char c): The input c is the next character typed by the user.
    The character will only be lower-case letters ('a' to 'z'), blank space (' ') or
    a special character ('#'). Also, the previously typed sentence should be recorded
    in your system. The output will be the top 3 historical hot sentences that have
    prefix the same as the part of sentence already typed.
    '''
    def __init__(self, sentences: List[str], times: List[int]):
        from collections import Counter
        self.tries = {}
        self.orig_sentences = Counter(dict(zip(sentences, times)))
        self.sentences = Counter(dict(zip(sentences, times)))
        self.stem = ""
        self.index = 0
        return


    def reset(self):
        self.tries = {}
        self.sentences = self.orig_sentences.copy()
        self.stem = ""
        self.index = 0
        return


    def compute_candidate_set(self, index):
        candidates = Counter(dict([(s, self.sentences[s]) for s in self.sentences if s.startswith(self.stem)]))
        self.sentences = candidates
        self.tries[index] = candidates
        return self.sentences


    def input(self, c: str) -> List[str]:
        self.stem += (c if c != '#' else "")
        sentence_counter = self.compute_candidate_set(self.index)
        self.index += 1

        flat, c2s, sentences = [], {}, []
        for s in sentence_counter:
            cnt = sentence_counter[s]
            if cnt not in c2s: c2s[cnt] = []
            c2s[cnt].append(s)
        counts = sorted(list(c2s.keys()), reverse=True)
        for cnt in counts:
            sentences = sorted(c2s[cnt])
            flat.extend(sentences)

        if c == "#":
            update = dict(zip([self.stem], [1]))
            self.orig_sentences.update(Counter(update))
            self.reset()
            return []

        return flat[:3]



# Your AutocompleteSystem object will be instantiated and called as such:
# obj = AutocompleteSystem(sentences, times)
# param_1 = obj.input(c)
