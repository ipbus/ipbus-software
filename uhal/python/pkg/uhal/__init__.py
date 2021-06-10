
import sys

try:
    from ._core import *
except ImportError as e:
    message = 'Failed to load uHAL bindings.'
    message += '\nDetails: "{}"'

    if sys.version_info[0] > 2:
        exec('raise type(e)(message.format(e.msg)).with_traceback(sys.exc_info()[2]) from None')
    else:
        exec('raise type(e), message.format(e.message), sys.exc_info()[2]')


##################################################
# Pythonic additions to the ValWord_uint32 API

if sys.version_info[0] <= 2:
    def _ValWord_to_long(self):
        return long(int(self))
    ValWord_uint32.__long__ = _ValWord_to_long

ValWord_uint32.__index__ = ValWord_uint32.__int__


def _add_int_method_to_ValWord(method_name, unary=False):
    if unary:
        def valWord_method(self):
            int_method = getattr(type(int(self)), method_name)
            return int_method( int(self) )
    else:
        def valWord_method(self, other):
            int_type = int
            if (sys.version_info[0] <= 2) and (isinstance(int(self), long) or ( not isinstance(other, str) and isinstance(int(other), long) )):
                int_type = long
            int_method = getattr(int_type, method_name)
            if isinstance(other, int_type) or isinstance(other, str):
                return int_method( int_type(self), other )
            else:
                return int_method( int_type(self), int_type(other) )

    # Add wraparound method to ValWord_uint32
    setattr(ValWord_uint32, method_name, valWord_method)


def _add_int_methods_to_ValWord(method_names, unary=False):
    for method_name in method_names:
        _add_int_method_to_ValWord(method_name, unary)


# Unary numeric methods
_add_int_methods_to_ValWord(['__invert__', '__neg__', '__pos__'], unary=True)

# Binary numeric methods
_add_int_methods_to_ValWord(['__add__', '__radd__',
                             '__sub__', '__rsub__',
                             '__mul__', '__rmul__',
                             '__mod__', '__rmod__',
                             '__pow__', '__rpow__',
                             '__lshift__', '__rlshift__',
                             '__rshift__', '__rrshift__',
                             '__and__', '__rand__',
                             '__or__', '__ror__',
                             '__xor__', '__rxor__'
                             ])

if sys.hexversion >= 0x020600F0:
    _add_int_method_to_ValWord('__format__')

# Unary comparison operator (used in "if valWord")
_add_int_method_to_ValWord('__bool__' if (sys.version_info[0] > 2) else '__nonzero__', unary=True)

# Binary comparison operators
_add_int_methods_to_ValWord(['__lt__', '__le__', '__eq__', '__ne__', '__gt__', '__ge__'] if (sys.version_info[0] > 2) else ['__cmp__'])

