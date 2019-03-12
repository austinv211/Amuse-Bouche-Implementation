from typing import List, Tuple, Optional, TypeVar
from toolz.functoolz import compose
# Haskell Translation
a = TypeVar("a")

def runLengthEncode(lst: List[a]) -> List[Tuple[a, int]]:
    if not lst:
        return []

    def nextGroup(value, streak, rest):
        if not rest:
            return [(value, streak)]

        return nextGroup(value, (streak + 1), rest[1:]) \
            if rest[0] == value \
            else [(value, streak)] + nextGroup(rest[0], 1, rest[1:])
    return nextGroup(lst[0], 1, lst[1:])

# Optimized
def runLengthEncodeIter(lst: List[a]) -> List[Tuple[a, int]]:
    if not lst:
        return []

    compressed = []
    last = lst[0]
    count = 0
    for ch in lst:
        if ch == last:
            count += 1
        else:
            compressed.append((last, count))
            last = ch
            count = 1

    compressed.append((last, count))
    return compressed


print(runLengthEncode("abccdbba"))


def rlePropLengthPreserved(lst: List[int]) -> bool:
    return len(lst) == sum(out[1] for out in runLengthEncode(lst))

print(rlePropLengthPreserved("abccdbba"))


def rlePropDupesCollapsed(n: int) -> bool:
    m = n % 100
    return runLengthEncode('x'*m) == [('x', m)] \
        if m != 0 \
        else runLengthEncode('') == []


print(rlePropDupesCollapsed(50))
print(rlePropDupesCollapsed(100))


def rlePropRoundTrip(lst: List[int]) -> bool:
    aNum = ord('a')
    letters = (chr( aNum + i ) for i in range( len( lst ) ) )
    compressed = compose(list, zip) (letters, (n % 100 + 1 for n in lst))
    unCompressed = ''.join( val[0] * val[1] for val in compressed )
    return runLengthEncode( unCompressed ) == compressed


print(rlePropRoundTrip([i for i in range(26)]))
