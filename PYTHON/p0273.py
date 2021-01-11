class Solution:
    '''
    Convert a non-negative integer num to its English words representation.
    '''
    def numberToWords(self, num: int) -> str:
        digits = str(num)
        n = len(digits)

        magn = dict(zip([0,1,2,3,4,5,6,7,8,9,10],
            ['', '', 'Hundred', 'Thousand', 'Thousand', 'Thousand', 'Million', 'Million', 'Million', 'Billion', 'Billion']))

        tens =      {'2': 'Twenty', '3': 'Thirty', '4': 'Forty',
                     '5': 'Fifty',  '6': 'Sixty',  '7': 'Seventy',
                     '8': 'Eighty', '9': 'Ninety', "0": "", "1": 'Ten' }
        teens =     {'11': 'Eleven',  '12': 'Twelve', '13': 'Thirteen',
                     '14': 'Fourteen', '15': 'Fifteen', '16': 'Sixteen',
                     '17': 'Seventeen', '18': 'Eighteen', '19': 'Nineteen', '10': 'Ten'}
        singles =   {'1': 'One', '2': 'Two', '3': 'Three', '4': 'Four',
                     '5': 'Five', '6': 'Six', '7': 'Seven', '8': 'Eight',
                     '9': 'Nine', '0': "Zero"}

        if n == 0:
            return "Zero"

        if n == 1:
            d = digits[0]
            return singles[d]


        def get_3dgroups(digits):
            groups = []
            pos = 0
            while digits:
                stem = digits[-3:]
                digits = digits[:-3]
                groups.append((stem, pos))
                pos += 3
            return groups


        def decode_group(group, pos=0):
            m = len(group)
            if m == 0:
                return ""

            group = group[::-1]

            g0, g1, g2 = "", "", ""
            d0, d1, d2 = "", "", ""
            if m >= 1: g0 = group[0]
            if m >= 2: g1 = group[1]
            if m >= 3: g2 = group[2]
            hundreds = "Hundred" if int(g2+g1+g0) > 99 else ""

            digits = [int(d) for d in group if d]
            if not sum(digits):
                return ""

            if m == 1 or (g1 == "0" and m == 2) or (g2 == "0" and g1 == "0" and m == 3):
                d0 = singles[g0]
                return process_decodings(d0, d1, d2, hundreds, pos)

            if m == 2 or (g2 == "0" and m == 3):
                if g0 == "0":
                    d0 = ""
                    d1 = tens[g1]
                    return process_decodings(d0, d1, d2, hundreds, pos)
                if g1 == "1":
                    d0 = ""
                    d1 = teens[g1+g0]
                    return process_decodings(d0, d1, d2, hundreds, pos)
                else:
                    d0 = singles[g0]
                    d1 = tens[g1]
                    return process_decodings(d0, d1, d2, hundreds, pos)

            if m == 3:
                d2 = singles[g2]
                d0 = singles[g0]
                if g0 == "0":
                    d0 = ""
                    d1 = tens[g1]
                    return process_decodings(d0, d1, d2, hundreds, pos)
                if g1 == "1":
                    d0 = ""
                    d1 = teens[g1+g0]
                    return process_decodings(d0, d1, d2, hundreds, pos)
                else:
                    d0 = singles[g0]
                    d1 = tens[g1]
                    return process_decodings(d0, d1, d2, hundreds, pos)


        def process_decodings(d0, d1, d2, hundreds, pos):
            if d0 != "": d0 += " "
            if d1 != "": d1 += " "
            if d2 != "": d2 += " "
            if hundreds != "": hundreds += " "
            out = "%s%s%s%s%s" % (d2, hundreds, d1, d0, magn[pos])
            return out

        groups = get_3dgroups(digits)
        groups = groups[::-1]

        out = ""
        while groups:
            group, pos = groups.pop()
            ret = decode_group(group, pos)
            out = ret + " " + out
            out = out.replace("  ", " ")

        return out.strip()




