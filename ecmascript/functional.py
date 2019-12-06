from abc import ABCMeta, abstractmethod
from typing import cast, Any, TypeVar, Generic, Callable, Union, Optional
from functools import partial, wraps, update_wrapper
from inspect import getfullargspec, FullArgSpec

A = TypeVar("A")
B = TypeVar("B")
C = TypeVar("C")


class Functor(Generic[A], metaclass=ABCMeta):
    @abstractmethod
    def map(self, f: Callable[[Optional[A]], B]) -> "Functor[B]":
        ...


class Maybe(Functor[A]):
    @staticmethod
    def of(val: Optional[B]) -> "Maybe[B]":
        return Nothing() if val is None else Just(val)

    def map(self, f: Callable[[Optional[A]], B]) -> "Maybe[B]":
        raise TypeError("map() called on type (rather than value)")

    def chain(self, f: Callable[[Optional[A]], B]) -> "Optional[B]":
        return self.map(f).join()

    def ap(self, f: Functor[B]) -> Functor[C]:
        raise TypeError("ap() called on type (rather than value)")

    def join(self) -> A:
        raise TypeError("join() called on type (rather than value)")

    def __bool__(self) -> bool:
        return not self.isNothing

    @property
    def isNothing(self) -> bool:
        raise TypeError("isNothing called for type (rather than value)")


class Nothing(Maybe[Any]):
    isNothing = True

    def __init__(self) -> None:
        pass

    def __repr__(self) -> str:
        return "Nothing"

    @staticmethod
    def of(val: Optional[B]) -> "Maybe[B]":
        raise TypeError("'of' called on class Nothing (value) instead of Maybe2 (type)")

    def map(self, f: Callable[[Optional[A]], B]) -> "Nothing":
        return self

    def ap(self, f: Functor[B]) -> "Nothing":
        return self

    def join(self) -> None:
        return None


class Just(Maybe[A]):
    isNothing = False

    def __init__(self, val: A) -> None:
        self.value = val

    @staticmethod
    def of(val: Optional[B]) -> "Maybe[B]":
        raise TypeError("'of' called on class Just (value) instead of Maybe2 (type)")

    def map(self, f: Callable[[Optional[A]], B]) -> "Maybe[B]":
        return Maybe.of(f(self.value))

    def ap(self: "Just[Callable[[B], C]]", f: Functor[B]) -> Functor[C]:
        return f.map(self.value)

    def join(self) -> A:
        return self.value

    def __repr__(self) -> str:
        return f"Just({self.value})"


def curried(func: Callable[..., A]) -> Callable[..., A]:
    """A decorator that makes the function curried
    Usage example:
    >>> @curried
    ... def sum5(a, b, c, d, e):
    ...     return a + b + c + d + e
    ...
    >>> sum5(1)(2)(3)(4)(5)
    15
    >>> sum5(1, 2, 3)(4, 5)
    15
    >>> cat4 = curried(lambda a, b, c, d: f'{a}{b}{c}{d}')
    >>> cat4('a')('b')('c')('d')
    'abcd'
    >>> cat4('a', 'b')('c', 'd')
    'abcd'
    """

    @wraps(func)
    def _curried(*args: Any, **kwargs: Any) -> Union[A, Callable[..., A]]:
        f = func
        count = 0
        while isinstance(f, partial):
            if f.args:
                count += len(f.args)
            f = f.func

        spec = getfullargspec(f)

        if count >= len(spec.args) - len(args):
            return func(*args, **kwargs)

        para_func = partial(func, *args, **kwargs)
        update_wrapper(para_func, func)
        return curried(para_func)

    return _curried
