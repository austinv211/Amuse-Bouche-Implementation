# Name: Part3.py
# Description: Python implementation of part 3 code
# Author: Austin Vargason

from typing import Any, TypeVar, List, NewType, Mapping, Callable
from pyrsistent import PClass, field
from datetime import date, timedelta
from toolz.curried import curry

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
aWeekLaterTest = maybeAddAWeek("blah")

print(aWeekLater)
print(anInterestingDate)
print(aWeekLater1)
print(aWeekLaterTest)

# part 3b
tvShows = {1966:"Star Trek", 1969:"Monty Python's Flying Circus", 1989:"The Simpsons"}

def showForYear(year: int) -> Maybe:
    return myLookup(year, tvShows)









