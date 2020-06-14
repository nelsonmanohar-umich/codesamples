class Solution:
    ''' Validate if a given string can be interpreted as a decimal number.

        Some examples:
        "0" => true
        " 0.1 " => true
        "abc" => false
        "1 a" => false
        "2e10" => true
        " -90e3   " => true
        " 1e" => false
        "e3" => false
        " 6e-1" => true
        " 99e2.5 " => false
        "53.5e93" => true
        " --6 " => false
        "-+3" => false
        "95a54e53" => false

        Note: It is intended for the problem statement to be ambiguous.
        You should gather all requirements up front before implementing
        one. However, here is a list of characters that can be in a
        valid decimal number:

        Numbers 0-9
        Exponent - "e"
        Positive/negative sign - "+"/"-"
        Decimal point - "."
        Of course, the context of these characters also matters in the input.
    '''
    def isNumber(self, s: str) -> bool:
        s = s.strip()
        parts = [x for x in s.split("e")]
        b, e = "", ""
        if len(parts) > 2: return False
        if len(parts) == 2:
            b, e = parts[0].lstrip(), parts[1].strip()
        if len(parts) == 1:
            b, e = parts[0].lstrip(), ""
        if len(parts) == 0:
            return False
        if len(parts) == 2:
            if b and not e: return False

        digits = "0123456789"
        valid_b = list("0123456789+-.")
        valid_e = list("0123456789-+.")

        valid = False

        for c in b:
            if c not in valid_b: return False
        for c in e:
            if c not in valid_e: return False

        if not b and e: return False
        if not b and not e: return False

        def validate(part):
            if part:
                dig_present = False
                for digit in digits:
                    if digit in part:
                        dig_present = True
                        break
                if not dig_present:
                    return False
            for c in ".+-":
                if part.count(c) > 1: return False
                if part == c: return False
                if c!="." and part.find(c) > 0: return False
            #for p in ["-00", "+00", "-00", "00"]:
                #if part.startswith(p): return False
            if "-" in part and "+" in part: return False

            return True

        if not validate(b) or not validate(e): return False
        if e.find(".") >= 0: return False

        return True


