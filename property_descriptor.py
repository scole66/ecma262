"""
6.2.5 The Property Descriptor Specification Type

The Property Descriptor type is used to explain the manipulation and reification of Object property attributes. Values
of the Property Descriptor type are Records. Each field's name is an attribute name and its value is a corresponding
attribute value as specified in 6.1.7.1. In addition, any field may be present or absent. The schema name used within
this specification to tag literal descriptions of Property Descriptor records is "PropertyDescriptor".

Property Descriptor values may be further classified as data Property Descriptors and accessor Property Descriptors
based upon the existence or use of certain fields. A data Property Descriptor is one that includes any fields named
either [[Value]] or [[Writable]]. An accessor Property Descriptor is one that includes any fields named either [[Get]]
or [[Set]]. Any Property Descriptor may have fields named [[Enumerable]] and [[Configurable]]. A Property Descriptor
value may not be both a data Property Descriptor and an accessor Property Descriptor; however, it may be neither. A
generic Property Descriptor is a Property Descriptor value that is neither a data Property Descriptor nor an accessor
Property Descriptor. A fully populated Property Descriptor is one that is either an accessor Property Descriptor or a
data Property Descriptor and that has all of the fields that correspond to the property attributes defined in either
Table 2 or Table 3.
"""

class PropertyDescriptor:
    def is_accessor_descriptor(self):
        "Returns True if this descriptor is an accessor style descriptor."
        # 2. If both Desc.[[Get]] and Desc.[[Set]] are absent, return false.
        # 3. Return true.
        return hasattr(self, 'Get') or hasattr(self, 'Set')

    def is_data_descriptor(self):
        "Returns True if this descriptor is a data style descriptor."
        # 2. If both Desc.[[Value]] and Desc.[[Writable]] are absent, return false.
        # 3. Return true.
        return hasattr(self, 'value') or hasattr(self, 'writable')

    def is_generic_descriptor(self):
        "Returns True if this descriptor cannot be described as either a accessor or data descriptor."
        # 2. If IsAccessorDescriptor(Desc) and IsDataDescriptor(Desc) are both false, return true.
        # 3. Return false.
        return not (self.is_accessor_descriptor() or self.is_data_descriptor())

    def complete_property_descriptor(self):
        "Fills in missing fields for a property descriptor."
        # 2. Let like be Record { [[Value]]: undefined, [[Writable]]: false, [[Get]]: undefined, [[Set]]: undefined,
        #    [[Enumerable]]: false, [[Configurable]]: false }.
        # 3. If IsGenericDescriptor(Desc) is true or IsDataDescriptor(Desc) is true, then
        if self.is_data_descriptor() or self.is_generic_descriptor():
            # a. If Desc does not have a [[Value]] field, set Desc.[[Value]] to like.[[Value]].
            if not hasattr(self, 'value'):
                self.value = None
            # b. If Desc does not have a [[Writable]] field, set Desc.[[Writable]] to like.[[Writable]].
            if not hasattr(self, 'writable'):
                self.writable = False
        # 4. Else,
        else:
            # a. If Desc does not have a [[Get]] field, set Desc.[[Get]] to like.[[Get]].
            if not hasattr(self, 'Get'):
                self.Get = None
            # b. If Desc does not have a [[Set]] field, set Desc.[[Set]] to like.[[Set]].
            if not hasattr(self, 'Set'):
                self.Set = None
        # 5. If Desc does not have an [[Enumerable]] field, set Desc.[[Enumerable]] to like.[[Enumerable]].
        if not hasattr(self, 'enumerable'):
            self.enumerable = False
        # 6. If Desc does not have a [[Configurable]] field, set Desc.[[Configurable]] to like.[[Configurable]].
        if not hasattr(self, 'configurable'):
            self.configurable = False
        # 7. Return Desc.
        return self

# 6.2.5.1 IsAccessorDescriptor ( Desc )
def IsAccessorDescriptor(desc):
    # When the abstract operation IsAccessorDescriptor is called with Property Descriptor Desc, the following steps are
    # taken:
    #
    # 1. If Desc is undefined, return false.
    if desc is None:
        return False
    return desc.is_accessor_descriptor()

# 6.2.5.2 IsDataDescriptor ( Desc )
def IsDataDescriptor(desc):
    # When the abstract operation IsDataDescriptor is called with Property Descriptor Desc, the following steps are
    # taken:
    #
    # 1. If Desc is undefined, return false.
    if desc is None:
        return False
    return desc.is_data_descriptor()

# 6.2.5.3 IsGenericDescriptor ( Desc )
def IsGenericDescriptor(desc):
    # When the abstract operation IsGenericDescriptor is called with Property Descriptor Desc, the following steps are
    # taken:
    #
    # 1. If Desc is undefined, return false.
    if desc is None:
        return False
    return desc.is_generic_descriptor()

# 6.2.5.4 FromPropertyDescriptor ( Desc )
def FromPropertyDescriptor(desc):
    # When the abstract operation FromPropertyDescriptor is called with Property Descriptor Desc, the following steps
    # are taken:
    #
    # 1. If Desc is undefined, return undefined.
    if desc is None:
        return None
    # 2. Let obj be ObjectCreate(%ObjectPrototype%).
    obj = ObjectCreate(intrinsics.ObjectPrototype)
    # 3. Assert: obj is an extensible ordinary object with no own properties.
    # 4. If Desc has a [[Value]] field, then
    crs = []
    if hasattr(desc, 'value'):
        # a. Perform CreateDataProperty(obj, "value", Desc.[[Value]]).
        cr = CreateDataProperty(obj, 'value', desc.value)
        crs.append(cr)
    # 5. If Desc has a [[Writable]] field, then
    if hasattr(desc, 'writable'):
        # a. Perform CreateDataProperty(obj, "writable", Desc.[[Writable]]).
        cr = CreateDataProperty(obj, 'writable', desc.writable)
        crs.append(cr)
    # 6. If Desc has a [[Get]] field, then
    if hasattr(desc, 'Get'):
        # a. Perform CreateDataProperty(obj, "get", Desc.[[Get]]).
        cr = CreateDataProperty(obj, 'get', desc.Get)
        crs.append(cr)
    # 7. If Desc has a [[Set]] field, then
    if hasattr(desc, 'Set'):
        # a. Perform CreateDataProperty(obj, "set", Desc.[[Set]]).
        cr = CreateDataProperty(obj, 'set', desc.Set)
        crs.append(cr)
    # 8. If Desc has an [[Enumerable]] field, then
    if hasattr(desc, 'enumerable'):
        # a. Perform CreateDataProperty(obj, "enumerable", Desc.[[Enumerable]]).
        cr = CreateDataProperty(obj, 'enumerable', desc.enumerable)
        crs.append(cr)
    # 9. If Desc has a [[Configurable]] field, then
    if hasattr(desc, 'configurable'):
        # a. Perform CreateDataProperty(obj, "configurable", Desc.[[Configurable]]).
        cr = CreateDataProperty(obj, 'configurable', desc.configurable)
        crs.append(cr)
    # 10. Assert: All of the above CreateDataProperty operations return true.
    assert all(cr.ctype == CompletionType.NORMAL and cr.value for cr in crs)
    # 11. Return obj.
    return obj

# 6.2.5.5 ToPropertyDescriptor ( Obj )
def ToPropertyDescriptor(obj):
    # When the abstract operation ToPropertyDescriptor is called with object Obj, the following steps are taken:
    #
    # 1. If Type(Obj) is not Object, throw a TypeError exception.
    if not isObject(obj):
        return ThrowCompletion(CreateTypeError())
    # 2. Let desc be a new Property Descriptor that initially has no fields.
    desc = PropertyDescriptor()
    # 3. Let hasEnumerable be ? HasProperty(Obj, "enumerable").
    has_enumerable = HasProperty(obj, 'enumerable')
    if has_enumerable.ctype != CompletionType.NORMAL:
        return has_enumerable
    # 4. If hasEnumerable is true, then
    if has_enumerable.value:
        # a. Let enum be ToBoolean(? Get(Obj, "enumerable")).
        enumble = Get(obj, 'enumerable')
        if enumble.ctype != CompletionType.NORMAL:
            return enumble
        # b. Set desc.[[Enumerable]] to enum.
        desc.enumerable = ToBoolean(enumble.value)
    # 5. Let hasConfigurable be ? HasProperty(Obj, "configurable").
    has_configurable = HasProperty(obj, 'configurable')
    if has_configurable.ctype != CompletionResult.NORMAL:
        return has_configurable
    # 6. If hasConfigurable is true, then
    if has_configurable.value:
        # a. Let conf be ToBoolean(? Get(Obj, "configurable")).
        conf = Get(obj, 'configurable')
        if conf.ctype != CompletionType.NORMAL:
            return conf
        # b. Set desc.[[Configurable]] to conf.
        desc.configurable = ToBoolean(conf.value)
    # 7. Let hasValue be ? HasProperty(Obj, "value").
    has_value = HasProperty(obj, 'value')
    if has_value.ctype != CompletionType.NORMAL:
        return has_value
    # 8. If hasValue is true, then
    if has_value.value:
        # a. Let value be ? Get(Obj, "value").
        value = Get(obj, 'value')
        if value.ctype != CompletionType.NORMAL:
            return value
        # b. Set desc.[[Value]] to value.
        desc.value = value.value
    # 9. Let hasWritable be ? HasProperty(Obj, "writable").
    has_writable = HasProperty(obj, 'writable')
    if has_writable.ctype != CompletionType.NORMAL:
        return has_writable
    # 10. If hasWritable is true, then
    if has_writable.value:
        # a. Let writable be ToBoolean(? Get(Obj, "writable")).
        writable = Get(obj, 'writable')
        if writable.ctype != CompletionType.NORMAL:
            return writable
        # b. Set desc.[[Writable]] to writable.
        desc.writable = ToBoolean(writable.value)
    # 11. Let hasGet be ? HasProperty(Obj, "get").
    has_get = HasProperty(obj, 'get')
    if has_get.ctype != CompletionType.NORMAL:
        return has_get
    # 12. If hasGet is true, then
    if has_get.value:
        # a. Let getter be ? Get(Obj, "get").
        getter = Get(obj, 'get')
        if getter.ctype != CompletionType.NORMAL:
            return getter
        # b. If IsCallable(getter) is false and getter is not undefined, throw a TypeError exception.
        if not IsCallable(getter.value) and getter.value is not None:
            return ThrowCompletion(CreateTypeError())
        # c. Set desc.[[Get]] to getter.
        desc.Get = getter.value
    # 13. Let hasSet be ? HasProperty(Obj, "set").
    has_set = HasProperty(obj, 'set')
    if has_set.ctype != CompletionType.NORMAL:
        return has_set
    # 14. If hasSet is true, then
    if has_set.value:
        # a. Let setter be ? Get(Obj, "set").
        setter = Get(obj, 'set')
        if setter.ctype != CompletionType.NORMAL:
            return setter
        # b. If IsCallable(setter) is false and setter is not undefined, throw a TypeError exception.
        if not IsCallable(setter.value) and setter.value is not None:
            return ThrowCompletion(CreateTypeError())
        # c. Set desc.[[Set]] to setter.
        desc.Set = setter.value
    # 15. If desc.[[Get]] is present or desc.[[Set]] is present, then
    if hasattr(desc, 'Get') or hasattr(desc, 'Set'):
        # a. If desc.[[Value]] is present or desc.[[Writable]] is present, throw a TypeError exception.
        if hasattr(desc, 'value') or hasattr(desc, 'writable'):
            return ThrowCompletion(CreateTypeError())
    # 16. Return desc.
    return NormalCompletion(desc)

# 6.2.5.6 CompletePropertyDescriptor ( Desc )
def CompletePropertyDescriptor(desc):
    # When the abstract operation CompletePropertyDescriptor is called with Property Descriptor Desc, the following
    # steps are taken:
    #
    # 1. Assert: Desc is a Property Descriptor.
    assert isinstance(desc, PropertyDescriptor)
    return desc.complete_property_descriptor()
