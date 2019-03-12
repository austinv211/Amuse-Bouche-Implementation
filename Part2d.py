from typing import Optional

def pickMessage(value: int) -> None:
    print(f"Pick a number, like {value}." if value is not None else "Pick any number you like")

def findAfterStarRec(str: str) -> Optional[str]:
    if len(str) <= 1:
        return None

    return str[1] if str[0] == '*' else findAfterStarRec(str[1:])

print(findAfterStarRec("*hello"))


def findAfterStarIter(str: str) -> Optional[str]:
    if len(str) <= 1:
        return None

    last = None
    for c in str:  # Cleaner than i in range(len(str) - 1)
        if last is '*':
            return c
        last = c
    return None

print(findAfterStarIter("h*ell*o"))


def findAfterCharRecr(ch: str, str: str) -> Optional[str]:
    if len(str) <= 1:
        return None

    return str[1] if str[0] == ch else findAfterCharRecr(ch, str[1:])

print(findAfterCharRecr('e', "hello"))


def findAfterCharIter(ch: str, str: str) -> Optional[str]:
    if len(str) <= 1:
        return None

    last = None
    for c in str:  # Cleaner than i in range(len(str) - 1)
        if last is ch:
            return c
        last = c
    return None


print(findAfterCharIter('z', "hell*o"))
