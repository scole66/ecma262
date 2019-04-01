# Section 6.2.4
#
# The Reference Specification Type
#
# NOTE  | The Reference type is used to explain the behaviour of such operators as delete, typeof, the assignment
#       | operators, the super keyword and other language features. For example, the left-hand operand of an assignment
#       | is expected to produce a reference.
#
# A Reference is a resolved name or property binding. A Reference consists of three components, the base value
# component, the referenced name component, and the Boolean-valued strict reference flag. The base value component is
# either undefined, an Object, a Boolean, a String, a Symbol, a Number, or an Environment Record. A base value
# component of undefined indicates that the Reference could not be resolved to a binding. The referenced name component
# is a String or Symbol value.
#
# A Super Reference is a Reference that is used to represent a name binding that was expressed using the super keyword.
# A Super Reference has an additional thisValue component, and its base value component will never be an Environment
# Record.

class Reference:
    def __init__(self, base, name, strict):
        self.base = base
        self.name = name
        self.strict = strict

# 6.2.4.1 GetBase ( V )
def GetBase(value):
    # 1. Assert: Type(V) is Reference.
    assert isinstance(value, Reference)
    # 2. Return the base value component of V.
    return value.base

# 6.2.4.2 GetReferencedName ( V )
def GetReferencedName(value):
    # 1. Assert: Type(V) is Reference.
    assert isinstance(value, Reference)
    # 2. Return the referenced name component of V.
    return value.name

# 6.2.4.3 IsStrictReference ( V )
def IsStrictReference(value):
    # 1. Assert: Type(V) is Reference.
    assert isinstance(value, Reference)
    # 2. Return the strict reference flag of V.
    return value.strict

# 6.2.4.4 HasPrimitiveBase ( V )
def HasPrimitiveBase(value):
    # 1. Assert: Type(V) is Reference.
    assert isinstance(value, Reference)
    # 2. If Type(V's base value component) is Boolean, String, Symbol, or Number, return true; otherwise return false.
    return IsBoolean(value.base) or IsString(value.base) or IsSymbol(value.base) or IsNumber(value.base)

# 6.2.4.5 IsPropertyReference ( V )
def IsPropertyReference(value):
    # 1. Assert: Type(V) is Reference.
    assert isinstance(value, Reference)
    # 2. If either the base value component of V is an Object or HasPrimitiveBase(V) is true, return true; otherwise
    #    return false.
    return IsObject(value.base) or HasPrimitiveBase(value)

# 6.2.4.6 IsUnresolvableReference ( V )
def IsUnresolvableReference(value):
    # 1. Assert: Type(V) is Reference.
    assert isinstance(value, Reference)
    # 2. If the base value component of V is undefined, return true; otherwise return false.
    return value.base is None

# 6.2.4.7 IsSuperReference ( V )
def IsSuperReference(value):
    # 1. Assert: Type(V) is Reference.
    assert isinstance(value, Reference)
    # 2. If V has a thisValue component, return true; otherwise return false.
    return hasattr(value, 'this_value')

# 6.2.4.8 GetValue ( V )
def GetValue(value):
    # 1. ReturnIfAbrupt(V).
    value, ok = ec(value)
    if not ok:
        return value
    # 2. If Type(V) is not Reference, return V.
    if not isinstance(value, Reference):
        return NormalCompletion(value)
    # 3. Let base be GetBase(V).
    base = GetBase(value)
    # 4. If IsUnresolvableReference(V) is true, throw a ReferenceError exception.
    if IsUnresolvableReference(value):
        return ThrowCompletion(CreateReferenceError())
    # 5. If IsPropertyReference(V) is true, then
    if IsPropertyReference(value):
        # a. If HasPrimitiveBase(V) is true, then
        if HasPrimitiveBase(value):
            # i. Assert: In this case, base will never be undefined or null.
            assert base is not None and not IsNull(base)
            # ii. Set base to ! ToObject(base).
            base = nc(ToObject(base))
        # b. Return ? base.[[Get]](GetReferencedName(V), GetThisValue(V)).
        return base.Get(GetReferencedName(value), GetThisValue(value))
    # 6. Else base must be an Environment Record,
    # a. Return ? base.GetBindingValue(GetReferencedName(V), IsStrictReference(V)) (see 8.1.1).
    return base.GetBindingValue(GetReferencedName(value), IsStrictReference(value))
    # NOTE
    # The object that may be created in step 5.a.ii is not accessible outside of the above abstract operation and the
    # ordinary object [[Get]] internal method. An implementation might choose to avoid the actual creation of the
    # object.

# 6.2.4.9 PutValue ( V, W )
def PutValue(ref, value):
    # 1. ReturnIfAbrupt(V).
    ref, ok = ec(ref)
    if not ok:
        return ref
    # 2. ReturnIfAbrupt(W).
    value, ok = ec(ref)
    if not ok:
        return ref
    # 3. If Type(V) is not Reference, throw a ReferenceError exception.
    if not isinstance(ref, Reference):
        return ThrowCompletion(CreateReferenceError())
    # 4. Let base be GetBase(V).
    base = GetBase(ref)
    # 5. If IsUnresolvableReference(V) is true, then
    if IsUnresolvableReference(ref):
        # a. If IsStrictReference(V) is true, then
        if IsStrictReference(ref):
            # i. Throw a ReferenceError exception.
            return ThrowCompletion(CreateReferenceError())
        # b. Let globalObj be GetGlobalObject().
        global_obj = GetGlobalObject()
        # c. Return ? Set(globalObj, GetReferencedName(V), W, false).
        return Set(global_obj, GetReferencedName(ref), value, False)
    # 6. Else if IsPropertyReference(V) is true, then
    elif IsPropertyReference(ref):
        # a. If HasPrimitiveBase(V) is true, then
        if HasPrimitiveBase(ref):
            # i. Assert: In this case, base will never be undefined or null.
            assert base is not None and not IsNull(base)
            # ii. Set base to ! ToObject(base).
            base = nc(ToObject(base))
        # b. Let succeeded be ? base.[[Set]](GetReferencedName(V), W, GetThisValue(V)).
        succeeded, ok = ec(base.Set(GetReferencedName(ref), value, GetThisValue(ref)))
        if not ok:
            return succeeded
        # c. If succeeded is false and IsStrictReference(V) is true, throw a TypeError exception.
        if not succeeded and IsStrictReference(ref):
            return ThrowCompletion(CreateTypeError())
        # d. Return.
        return NormalCompletion(None)
    # 7. Else base must be an Environment Record,
    # a. Return ? base.SetMutableBinding(GetReferencedName(V), W, IsStrictReference(V)) (see 8.1.1).
    return base.SetMutableBinding(GetReferencedName(ref), value, IsStrictReference(ref))
    # NOTE
    # The object that may be created in step 6.a.ii is not accessible outside of the above algorithm and the ordinary
    # object [[Set]] internal method. An implementation might choose to avoid the actual creation of that object.

# 6.2.4.10 GetThisValue ( V )
def GetThisValue(ref):
    # 1. Assert: IsPropertyReference(V) is true.
    assert IsPropertyReference(ref)
    # 2. If IsSuperReference(V) is true, then
    if IsSuperReference(ref):
        # a. Return the value of the thisValue component of the reference V.
        return ref.this_value
    # 3. Return GetBase(V).
    return GetBase(ref)

# 6.2.4.11 InitializeReferencedBinding ( V, W )
def InitializeReferencedBinding(ref, value):
    # 1. ReturnIfAbrupt(V).
    ref, ok = ec(ref)
    if not ok:
        return ref
    # 2. ReturnIfAbrupt(W).
    value, ok = ec(value)
    if not ok:
        return value
    # 3. Assert: Type(V) is Reference.
    assert isinstance(ref, Reference)
    # 4. Assert: IsUnresolvableReference(V) is false.
    assert not IsUnresolvableReference(ref)
    # 5. Let base be GetBase(V).
    base = GetBase(ref)
    # 6. Assert: base is an Environment Record.
    # 7. Return base.InitializeBinding(GetReferencedName(V), W).
    return base.InitializeBinding(GetReferencedName(ref), value)
