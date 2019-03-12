# Name: Part3.py
# Description: Python implementation of part 3 code
# Author: Austin Vargason

from typing import Any, TypeVar, List, NewType, Mapping, Callable, Tuple
from pyrsistent import PClass, field
from datetime import date, timedelta
from toolz import curry, compose
from functools import partial

#defining what a just is
class Just(PClass):
    a = field()

class Nothing(PClass):
    None

# defining what a maybe is
Maybe = TypeVar('Maybe', Just, Nothing)


#make an A type variable
A = TypeVar('A')

#elemIndex function
def elemIndex(element: A, values: List[A]) -> Maybe:
    try:
        result = values.index(element)
        return Just(a=result)
    except:
        return Nothing()

# make a k type variable
K = TypeVar('K')

# lookup function
def myLookup(key: K, mapInput: Mapping[K, A]) -> Maybe:
    try:
        result = mapInput[key]
        return Just(a=result)
    except:
        return Nothing()

# Part 3a
def firstOne(values: List[A]) -> A:
    try:
        result = values[0]
        return result
    except:
        raise Exception("O Noes!")

def firstOneMaybe(values: List[A]) -> Maybe:
    try:
        result = values[0]
        return Just(a=result)
    except:
        return Nothing()

def addAWeek(value: date) -> date:
    return value.a + timedelta(days=7)

# fmap
@curry
def fmap(function_list: [Callable], argument: A) -> [Maybe]:
 result = argument
 for function in function_list:
  #print "calling " + function.__name__ + "(" + repr(result) + ")"
  result = function(result)
 return result

interestingDates: List[date] = [date(1966, 9, 8), date(1969, 6, 21), date(1969, 10, 29)]

anInterestingDate: Maybe = firstOneMaybe(interestingDates)

aWeekLater = fmap([addAWeek], anInterestingDate)


def maybeAddAWeek(value: Maybe) -> Maybe:
    try:
        return fmap([addAWeek], value)
    except:
        return Nothing()

aWeekLater1 = maybeAddAWeek(anInterestingDate)
aWeekLaterBlah = maybeAddAWeek("blah")

# part 3b
tvShows = {1966:"Star Trek", 1969:"Monty Python's Flying Circus", 1989:"The Simpsons"}

def showForYear(year: int) -> Maybe:
    return myLookup(year, tvShows)

@curry
def listToMaybe(values: List[A]) -> [Maybe]:
    if (len(values) > 0):
        return [Just(a=res) if res != None else Nothing() for res in values]
    else:
        return Nothing()


def showWithName(showName: str) -> Maybe:
    curryFilter = curry(filter)
    return compose(listToMaybe , list, curryFilter(lambda k: showName in k)) (tvShows.values())

def favoriteShow(value: str) -> Maybe:
    if value == "Amy":
        return Just(a="Batman")
    elif value == "Bob":
        return Just(a="Iron Chef")
    else:
        return Nothing()

# define a person
class Person(PClass):
    name = field(type=str)
    year = field(type=int)

amy = Person(name="Amy", year=1971)
cam = Person(name="Cam", year=1989)
deb = Person(name="Deb", year=1967)
monty = Person(name="Monty", year=1973)

class Infix(object):
    def __init__(self, func):
        self.func = func
    def __or__(self, other):
        return self.func(other)
    def __ror__(self, other):
        return Infix(partial(self.func, other))
    def __call__(self, v1, v2):
        return self.func(v1, v2)

@Infix
def short(x, y):
    return x if not isinstance(x, Nothing) else y

def pickShow(value: Person) -> Maybe:
    return favoriteShow(value.name) |short| showWithName(value.name) |short| showForYear(value.year)
    

if __name__ == "__main__":
    print(f'aWeekLater: {aWeekLater}')
    print(f'anInterestingDate: {anInterestingDate}')
    print(f'aWeekLater1: {aWeekLater1}')
    print(f'aWeekLaterBlah: {aWeekLaterBlah}')
    print(f'\nPickShow Amy: {pickShow(amy)}')
    print(f'PickShow Cam:{pickShow(cam)}')
    print(f'PickShow Deb: {pickShow(deb)}')
    print(f'PickShow Monty: {pickShow(monty)}')



